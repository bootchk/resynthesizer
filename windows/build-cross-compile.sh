#!/bin/bash
# Cross-compile resynthesizer.exe for Windows (x86-64) from Linux.
#
# Uses a Fedora container with mingw-w64 + GIMP development headers
# extracted from MSYS2 packages. Produces a PE32+ Windows executable.
#
# Usage: bash build-cross-compile.sh
# Output: builddir/enginePlugin/resynthesizer.exe
#
# Dependencies: docker, curl, zstd
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"
WORK_DIR="/tmp/resynth-cross-build"
GIMP_VER="3.2.6"

echo "=== Resynthesizer Windows Cross-Compilation ==="
echo "Repo: $REPO_DIR"

mkdir -p "$WORK_DIR"
cd "$WORK_DIR"

# Download MSYS2 GIMP development package (headers + import libraries)
echo "--- Downloading GIMP $GIMP_VER dev files from MSYS2 ---"
if [ ! -f "msys2-gimp.pkg.tar.zst" ]; then
    curl -sL -o msys2-gimp.pkg.tar.zst \
        "https://mirror.msys2.org/mingw/ucrt64/mingw-w64-ucrt-x86_64-gimp-${GIMP_VER}-1-any.pkg.tar.zst"
fi

# Extract GIMP dev files
if [ ! -d "gimp-dev/ucrt64" ]; then
    mkdir -p gimp-dev
    tar --zstd -xf msys2-gimp.pkg.tar.zst -C gimp-dev
fi

# Download dependency packages (gegl, gexiv2, pango, babl, etc.)
DEPS="gegl gexiv2 pango babl lcms2 libpng harfbuzz freetype fontconfig libffi libxml2 libjpeg-turbo libwebp exiv2"
for dep in $DEPS; do
    full="mingw-w64-ucrt-x86_64-${dep}"
    if [ ! -f "${full}.pkg.tar.zst" ]; then
        echo "  Downloading $dep..."
        ver=$(curl -sL --max-time 10 "https://mirror.msys2.org/mingw/ucrt64/" \
            | grep -o "${full}-[0-9][^\"']*\.pkg\.tar\.zst" | sort -V | tail -1)
        if [ -n "$ver" ]; then
            curl -sL -o "${full}.pkg.tar.zst" "https://mirror.msys2.org/mingw/ucrt64/${ver}"
        fi
    fi
    if [ -f "${full}.pkg.tar.zst" ] && [ ! -d "gimp-dev/extract-${dep}" ]; then
        mkdir -p "gimp-dev/extract-${dep}"
        tar --zstd -xf "${full}.pkg.tar.zst" -C "gimp-dev/extract-${dep}" 2>/dev/null || true
    fi
done

# Write the build script for the container
cat > build.sh << 'BUILD_SCRIPT'
#!/bin/bash
set -e

SYSROOT="/usr/x86_64-w64-mingw32/sys-root/mingw"

echo "--- Installing Fedora mingw64 toolchain ---"
dnf install -y --setopt=install_weak_deps=False --skip-unavailable \
    mingw64-gcc-c++ mingw64-glib2 mingw64-gdk-pixbuf mingw64-cairo \
    mingw64-pango mingw64-gettext mingw64-pkg-config \
    meson ninja-build make zstd findutils diffutils 2>&1 | tail -3

echo "--- Copying GIMP dev files to sysroot ---"
cp -r /build/gimp-dev/ucrt64/include/gimp-3.0 $SYSROOT/include/
cp /build/gimp-dev/ucrt64/lib/libgimp*.dll.a $SYSROOT/lib/
cp /build/gimp-dev/ucrt64/lib/pkgconfig/gimp*-3.0.pc $SYSROOT/lib/pkgconfig/

for dep in gegl gexiv2 pango babl lcms2 libpng harfbuzz freetype fontconfig libffi libxml2 libjpeg-turbo libwebp exiv2; do
    [ -d "/build/gimp-dev/extract-${dep}/ucrt64/include" ] && \
        cp -r /build/gimp-dev/extract-${dep}/ucrt64/include/* $SYSROOT/include/ 2>/dev/null
    [ -d "/build/gimp-dev/extract-${dep}/ucrt64/lib" ] && \
        cp /build/gimp-dev/extract-${dep}/ucrt64/lib/*.dll.a $SYSROOT/lib/ 2>/dev/null
    [ -d "/build/gimp-dev/extract-${dep}/ucrt64/lib/pkgconfig" ] && \
        cp /build/gimp-dev/extract-${dep}/ucrt64/lib/pkgconfig/*.pc $SYSROOT/lib/pkgconfig/ 2>/dev/null
done

sed -i 's|prefix=/ucrt64|prefix='$SYSROOT'|' $SYSROOT/lib/pkgconfig/*.pc

# Minimal gimp-3.0.pc (bypasses heavy transitive deps for cross-compile)
cat > $SYSROOT/lib/pkgconfig/gimp-3.0.pc << 'EOF'
prefix=/usr/x86_64-w64-mingw32/sys-root/mingw
includedir=${prefix}/include
libdir=${prefix}/lib
Name: GIMP
Description: GIMP Library (minimal for cross-compile)
Version: 3.2.6
Requires: glib-2.0 gobject-2.0 gio-2.0 gdk-pixbuf-2.0
Libs: -L${libdir} -lgimp-3.0 -lgimpbase-3.0 -lgimpcolor-3.0 -lgimpconfig-3.0 -lgimpmath-3.0 -lgegl-0.4 -lbabl-0.1
Cflags: -I${includedir}/gimp-3.0 -I${includedir}/cairo -I${includedir} -I${includedir}/gegl-0.4 -I${includedir}/gexiv2 -I${includedir}/pango-1.0 -I${includedir}/babl-0.1 -I${includedir}/freetype2 -I${includedir}/harfbuzz -I${includedir}/libpng16
EOF

echo "--- Configuring meson cross-build ---"
cat > /build/cross-mingw64.ini << 'EOF'
[binaries]
c = 'x86_64-w64-mingw32-gcc'
cpp = 'x86_64-w64-mingw32-g++'
ar = 'x86_64-w64-mingw32-ar'
strip = 'x86_64-w64-mingw32-strip'
pkg-config = 'x86_64-w64-mingw32-pkg-config'

[host_machine]
system = 'windows'
cpu_family = 'x86_64'
cpu = 'x86_64'
endian = 'little'

[properties]
needs_exe_wrapper = true
EOF

export PKG_CONFIG_LIBDIR=$SYSROOT/lib/pkgconfig
export PKG_CONFIG_PATH=$SYSROOT/lib/pkgconfig

cd /build/resynth-src
rm -rf builddir
meson setup builddir \
    --cross-file /build/cross-mingw64.ini \
    -Dprefix=$SYSROOT \
    -Dsynth-use-glib=true \
    -Dsynth-use-glib-threads=true \
    -Dsynth-threaded=true \
    -Dinstall-test=false \
    -Dinstall-translations=false \
    -Dbuild-libheal=false

echo "--- Compiling ---"
meson compile -C builddir -v

echo "--- BUILD SUCCESSFUL ---"
ls -la builddir/enginePlugin/resynthesizer.exe
file builddir/enginePlugin/resynthesizer.exe
BUILD_SCRIPT

chmod +x build.sh

echo "--- Running build in Fedora container ---"
docker run --rm \
    -v "$WORK_DIR/build.sh:/build/build.sh:ro" \
    -v "$WORK_DIR/gimp-dev:/build/gimp-dev:ro" \
    -v "$REPO_DIR:/build/resynth-src" \
    fedora:latest bash /build/build.sh

echo ""
echo "=== Done! ==="
echo "Output: $REPO_DIR/builddir/enginePlugin/resynthesizer.exe"
echo ""
echo "To package: copy resynthesizer.exe + outerPlugins/*.scm into"
echo "individual named subfolders under GIMP's plug-ins directory."
echo "See windows/INSTALL-WINDOWS.md for the folder structure."
