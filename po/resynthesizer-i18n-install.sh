#!/bin/sh

# This is a custom install script 
# for the i18n data of the resynthesizer suite of plugins for GIMP.
# It is run by Meson during the installation phase,
# and is responsible for installing the compiled .mo files for translations
# to the appropriate directories where GIMP can find them at runtime.

# Requires the 'msgfmt' tool from GNU gettext to compile .po files into .mo files.

# The arguments passed to this script are:
# 1. LIBDIR: The destination directory for installation, platform-specific, 
# e.g., /usr/local/lib or /usr/lib 
# or some other temporary directory when using DESTDIR for subsequent packaging step.
# It comes from the Meson option 'libdir', 
# and Meson will automatically adjust it for the platform.

# This script also uses environment variables set by Meson 
# --MESON_INSTALL_DESTDIR_PREFIX-- the combined path of DESTDIR and MESON_INSTALL.
# You should not use DESTDIR directly in the script, but you can use MESON_INSTALL_DESTDIR_PREFIX.

echo ">>>>>>>>>>>Running custom install script for resynthesizer suite."
echo "Destination: ${MESON_INSTALL_DESTDIR_PREFIX}"
echo "Arg LIBDIR: $@"

# i18n data is installed as subdirectory of a plugin's root directory.
# This makes the installation "relocatable" 
# i.e. it is "bundled" with the plugin, and can be moved around with the plugin.
# Really should be a concern of packagers, and the old way-- installing
# to the platform's standard locale directory -- IMO should be supported by GIMP.
# The packagers should be responsible for bundling/sandboxing the plugins and their translations.
# For example, flatpak/snap/appImage should redirect a plugin's access to the standard locale directory
# to the bundled locale directory.

# Where GIMP installs system-wide plugins.
GimpPluginDir="${MESON_INSTALL_DESTDIR_PREFIX}/$1/gimp/3.0/plug-ins"
echo "GimpPluginDir: ${GimpPluginDir}"

# Make the destination directory if it doesn't exist (-p)
# The destination directory is the destination for one languages .mo file, for one plugin.
#     where GIMP installs plugins: 
#        libdir/
#        gimp/3.0/plug-ins/
#     a particular plugin root dir: resynthesizer/
#     where gettext looks for translations for a language: locale/<language code>/LC_MESSAGES

# Repeat the above for each plugin that needs translations 
# (8 total in this case, including the engine plugin), 
# and for each language you want to support (6 total in this case).

# The "engine" plugin is the "resynthesizer" plugin, 
# which is the core/helper/engine of the suite,
# and has translations for error messages.
mkdir -p $GimpPluginDir/resynthesizer/locale/cs/LC_MESSAGES
mkdir -p $GimpPluginDir/resynthesizer/locale/fr/LC_MESSAGES
mkdir -p $GimpPluginDir/resynthesizer/locale/pl/LC_MESSAGES
mkdir -p $GimpPluginDir/resynthesizer/locale/pt_BR/LC_MESSAGES
mkdir -p $GimpPluginDir/resynthesizer/locale/ru/LC_MESSAGES
mkdir -p $GimpPluginDir/resynthesizer/locale/tr/LC_MESSAGES

mkdir -p $GimpPluginDir/plug-in-heal-selection/locale/cs/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-heal-selection/locale/fr/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-heal-selection/locale/pl/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-heal-selection/locale/pt_BR/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-heal-selection/locale/ru/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-heal-selection/locale/tr/LC_MESSAGES

mkdir -p $GimpPluginDir/plug-in-heal-transparency/locale/cs/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-heal-transparency/locale/fr/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-heal-transparency/locale/pl/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-heal-transparency/locale/pt_BR/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-heal-transparency/locale/ru/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-heal-transparency/locale/tr/LC_MESSAGES

mkdir -p $GimpPluginDir/plug-in-uncrop/locale/cs/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-uncrop/locale/fr/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-uncrop/locale/pl/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-uncrop/locale/pt_BR/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-uncrop/locale/ru/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-uncrop/locale/tr/LC_MESSAGES

mkdir -p $GimpPluginDir/plug-in-map-style/locale/cs/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-map-style/locale/fr/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-map-style/locale/pl/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-map-style/locale/pt_BR/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-map-style/locale/ru/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-map-style/locale/tr/LC_MESSAGES

mkdir -p $GimpPluginDir/plug-in-render-texture/locale/cs/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-render-texture/locale/fr/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-render-texture/locale/pl/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-render-texture/locale/pt_BR/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-render-texture/locale/ru/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-render-texture/locale/tr/LC_MESSAGES

mkdir -p $GimpPluginDir/plug-in-resynth-fill-pattern/locale/cs/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-resynth-fill-pattern/locale/fr/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-resynth-fill-pattern/locale/pl/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-resynth-fill-pattern/locale/pt_BR/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-resynth-fill-pattern/locale/ru/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-resynth-fill-pattern/locale/tr/LC_MESSAGES

# The "controls" plugin is the plugin that provides a GUI 
# for the raw/bare/not-nested engine plugin
mkdir -p $GimpPluginDir/plug-in-resynth-controls/locale/cs/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-resynth-controls/locale/fr/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-resynth-controls/locale/pl/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-resynth-controls/locale/pt_BR/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-resynth-controls/locale/ru/LC_MESSAGES
mkdir -p $GimpPluginDir/plug-in-resynth-controls/locale/tr/LC_MESSAGES



echo "Installing Czech translations"
# Compile one languages .po file into a .mo file using msgfmt.
# The input is in the source directory, 
# and the output is a temporary .mo file in the current build directory, 
# which we will move to the final destination in the next step.
# The input is named for a language, e.g., 'cs' for Czech, 
# and the output is named 'resynthesizer3.mo' which is a "domain" name
# shared by the suite of plugins, and is the name that matches the domain name in the source code,
# that gettext uses to find the translations at runtime.
msgfmt -o resynthesizer3.mo ${MESON_SOURCE_ROOT}/po/cs.po

# The output was temporarily placed in the current build directory.
# Now we move it to the final destinations (one for each plugin)
cp resynthesizer3.mo $GimpPluginDir/resynthesizer/locale/cs/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-heal-selection/locale/cs/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-heal-transparency/locale/cs/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-uncrop/locale/cs/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-map-style/locale/cs/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-render-texture/locale/cs/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-resynth-fill-pattern/locale/cs/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-resynth-controls/locale/cs/LC_MESSAGES

# Repeat the above for each language

echo "Installing French translations"
msgfmt -o resynthesizer3.mo ${MESON_SOURCE_ROOT}/po/fr.po
cp resynthesizer3.mo $GimpPluginDir/resynthesizer/locale/fr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-heal-selection/locale/fr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-heal-transparency/locale/fr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-uncrop/locale/fr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-map-style/locale/fr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-render-texture/locale/fr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-resynth-fill-pattern/locale/fr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-resynth-controls/locale/fr/LC_MESSAGES

echo "Installing Polish translations"
msgfmt -o resynthesizer3.mo ${MESON_SOURCE_ROOT}/po/pl.po
cp resynthesizer3.mo $GimpPluginDir/resynthesizer/locale/pl/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-heal-selection/locale/pl/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-heal-transparency/locale/pl/LC_MESSAGES
cp resynthesizerer3.mo $GimpPluginDir/plug-in-uncrop/locale/pl/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-map-style/locale/pl/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-render-texture/locale/pl/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-resynth-fill-pattern/locale/pl/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-resynth-controls/locale/pl/LC_MESSAGES

echo "Installing Portuguese (Brazil) translations"
msgfmt -o resynthesizer3.mo ${MESON_SOURCE_ROOT}/po/pt_BR.po
cp resynthesizer3.mo $GimpPluginDir/resynthesizer/locale/pt_BR/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-heal-selection/locale/pt_BR/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-heal-transparency/locale/pt_BR/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-uncrop/locale/pt_BR/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-map-style/locale/pt_BR/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-render-texture/locale/pt_BR/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-resynth-fill-pattern/locale/pt_BR/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-resynth-controls/locale/pt_BR/LC_MESSAGES

echo "Installing Russian translations"
msgfmt -o resynthesizer3.mo ${MESON_SOURCE_ROOT}/po/ru.po
cp resynthesizer3.mo $GimpPluginDir/resynthesizer/locale/ru/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-heal-selection/locale/ru/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-heal-transparency/locale/ru/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-uncrop/locale/ru/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-map-style/locale/ru/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-render-texture/locale/ru/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-resynth-fill-pattern/locale/ru/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-resynth-controls/locale/ru/LC_MESSAGES

echo "Installing Turkish translations"
msgfmt -o resynthesizer3.mo ${MESON_SOURCE_ROOT}/po/tr.po
cp resynthesizer3.mo $GimpPluginDir/resynthesizer/locale/tr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-heal-selection/locale/tr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-heal-transparency/locale/tr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-uncrop/locale/tr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-map-style/locale/tr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-render-texture/locale/tr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-resynth-fill-pattern/locale/tr/LC_MESSAGES
cp resynthesizer3.mo $GimpPluginDir/plug-in-resynth-controls/locale/tr/LC_MESSAGES

echo "Finished installing translations for resynthesizer suite."