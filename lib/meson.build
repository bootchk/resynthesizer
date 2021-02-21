
# Optional, when independent of GLib
# 'glibProxy.c',

# Other .h files may have definitions as well as declarations (not true headers)

libresynthesizer_sources = [
  'buildSwitches.h',
  'imageSynth.c',
  'engine.c',
  'engineParams.c',
  'imageFormat.c',
  'progress.c',
]

# TODO
# declare headers for the library, so #include <> works for library users
libresynthesizer_headers = [
  'imageFormat.h',
]


# declare to install headers for libresynthesizer
install_headers(libresynthesizer_headers)




# define dependencies on libraries

# !!! Can be built independent of libgimp and glib
# But this is not it.

# libm
cc = meson.get_compiler('c')
math_dep = cc.find_library('m', required : false)

# glib
# Might work with earlier versions, I am choosing 2.54 somewhat arbitrarily
glib_dep = dependency('glib-2.0', version : '>=2.54.0')

# libgimp, at least the beta for v3.0
gimp_dep = dependency('gimp-3.0', version : '>=2.99.0')

# Declaring dependencies: takes care of linking.
# Not: link_with: [ math, ],
# version: so_version,  ??? what is this
# c_args '-DGIMP_COMPILATION',  ??? what

# define meson var resynth_lib to the locally built library
resynth_lib = library('libresynthesizer-3.0',
  libresynthesizer_sources,
  dependencies: [
    math_dep,
    glib_dep,
    gimp_dep,
  ],
  include_directories: resynthesizer_inc,
  c_args: [ '-DG_LOG_DOMAIN="LibResynthesizer"',  ],
  install: true,
)

# declare to install using pkgconfig
pkg_mod = import('pkgconfig')
pkg_mod.generate(libraries : resynth_lib,
                 version : '3.0',
                 name : 'libresynthesizer',
                 filebase : 'resynthesizer',
                 description : 'A library to inpaint images.')
