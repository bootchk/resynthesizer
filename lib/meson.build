# The engine, a library.
# Referenced by an engine plugin executable.

# engine is a 'convenience' (not installed), static library
# You could install the library shared, instead of linking with the engine plugin.

# Some .h files may have definitions as well as declarations (not true headers)

# !!! The library is independent of Gimp libraries

# The library does not need internationalization.





# libheal only does healing of a hole
libheal_sources = [
  'imageSynth.c', # <- this is the difference
  'engine.c',
  'engineParams.c',
  'imageFormat.c',
  'progress.c',
]

# libresynthesizer is much more general,
# heals and does texture transfer, etc.
libresynthesizer_sources = [
  'engine.c',
  'engineParams.c',
  'imageFormat.c',
  'progress.c',
  'stats.c',
  'targetPixels.c',
  'map.c',
  'targetPointsOrder.c',
  'coordinates.c',
  'bounds.c',
]

# declare headers for the library, so #include <> works for library users
#libresynthesizer_headers = [
#  'imageFormat.h',
#]

# declare to install headers for libresynthesizer
#install_headers(libresynthesizer_headers)




# define dependencies on libraries

# !!! Can be built independent of glib
# But this is not it.

# libm
cc = meson.get_compiler('c')
math_dep = cc.find_library('m', required : false)

# glib
if get_option('synth-use-glib')
  # Might work with earlier versions, choosing 2.54 somewhat
  glib_dep = dependency('glib-2.0', version : '>=2.54.0')

  mac_gimp_app = get_option('mac-gimp-app')
  if mac_gimp_app != ''
    # On macOS, we'll use the shared library from the GIMP App and
    # keep only the headers from the glib_dep we just found.
    mac_dylib_path = mac_gimp_app / 'Contents/Resources/lib'
    glib_dep = declare_dependency(
      compile_args: [
        # Current versions of GIMP.app use glib 2.78. If the headers we find
        # are for version >= 2.80, the G_DEFINE_TYPE macro uses a g_once_init
        # symbol that isn't available in GIMP.app's glib-2.0.dylib.
        # Here, we limit this to 2.54 <= version <= 2.79.
        # The value is defined in glib/gversionmacros.h as:
        #
        #     ((major << 16) | (minor << 8))
        #
        # The 2.54 lower bound comes from the depencency() call, above.
        '-DGLIB_VERSION_MIN_REQUIRED=0x23600',
        '-DGLIB_VERSION_MAX_ALLOWED=0x24f00',
      ],
      dependencies: [
        glib_dep.partial_dependency(compile_args: true),
        cc.find_library('glib-2.0', dirs: [mac_dylib_path], required: true),
      ],
    )
  endif
else
  glib_dep = dependency('', required: false)
  libresynthesizer_sources +=  'glibProxy.c'
endif


# define meson var resynth_lib to the locally built library
# !!! static and not installed
resynth_lib = static_library('libresynthesizer-3.0',
  libresynthesizer_sources,
  dependencies: [
    math_dep,
    glib_dep,
  ],
  include_directories: resynthesizer_inc,
  c_args: [ '-DG_LOG_DOMAIN="LibResynthesizer"',  ],
  install: false,
)

# libheal is usually just for testing.
# Often it is build without GLib.
# TODO make this more independent of GLib
# and not static and installed shared.
# That is the use case for libheal:
# a shared library on another platform.
if get_option('build-libheal')
  heal_lib = static_library('libheal-3.0',
    libheal_sources,
    dependencies: [
      math_dep,
      glib_dep,
    ],
    include_directories: resynthesizer_inc,
    c_args: [ '-DG_LOG_DOMAIN="LibHeal"',  ],
    install: false,
  )
endif

# declare to install using pkgconfig
#pkg_mod = import('pkgconfig')
#pkg_mod.generate(libraries : resynth_lib,
#                 version : '3.0',
#                 name : 'libresynthesizer',
#                 filebase : 'resynthesizer',
#                 description : 'A library to inpaint images.')
