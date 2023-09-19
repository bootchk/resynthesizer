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
