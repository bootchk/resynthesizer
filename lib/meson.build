# The engine, a library.
# Referenced by an engine plugin executable.

# engine is a 'convenience' (not installed), static library
# You could install the library shared, instead of linking with the engine plugin.

# Some .h files may have definitions as well as declarations (not true headers)

# !!! The library is independent of Gimp libraries

# The library does not need internationalization.


# Optional, when independent of GLib
# See buildswitches. TODO meson_options
# 'glibProxy.c',



libresynthesizer_sources = [
  'buildSwitches.h',
  'imageSynth.c',
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
# Might work with earlier versions, choosing 2.54 somewhat arbitrarily
glib_dep = dependency('glib-2.0', version : '>=2.54.0')





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

# declare to install using pkgconfig
#pkg_mod = import('pkgconfig')
#pkg_mod.generate(libraries : resynth_lib,
#                 version : '3.0',
#                 name : 'libresynthesizer',
#                 filebase : 'resynthesizer',
#                 description : 'A library to inpaint images.')
