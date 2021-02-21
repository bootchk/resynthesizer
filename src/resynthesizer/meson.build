
# Define meson variables

plugin_name = 'resynthesizer'

# assert defined earlier: gimpplugindir = '/usr/local/lib/x86_64-linux-gnu/gimp/2.99'


# !!! Other .h files have compilable code (they are not just headers)
# but we only need to list the top compiled files

resynthesizer_sources = [
  'resynthesizer.c',
  'plugin.c',
  'debug.c',
  'pluginParams.c',
  'drawable.c',
]



# define dependencies on libraries

# libgimp
# version 2.99 is the beta for v3.0
gimp_dep = dependency('gimp-3.0', version : '>=2.99.0')

# resynthesizer plugin calls the resynthesizer engine (libresynthesizer)
# which depends on math library libm

# Dependency on libresynthesizer is declared to be a linking dependency
# TODO
# resynthesizerEngine_dep = dependency('resynthesizer-3.0', version : '>=2.99.0')




# include_directories: [ rootInclude, ],
# dependencies: [ plugin_deps, math ],

#link_with : [
#  libgimp,
#],
#dependencies: [ gimp_dep, math_dep],


# Since 3.0 a plugin must install to a dir named the same as the plugin
# Hence: install_dir: ...

# This plugin uses libgimp
# The dependencies: declares that and takes care of includes and linking

executable(plugin_name,
  resynthesizer_sources,
  dependencies: [
    gimp_dep,
  ],
  c_args: [ '-DG_LOG_DOMAIN="Resynthesizer"', '-DGIMP_COMPILATION', ],
  link_with : [
    resynth_lib,
  ],
  include_directories: resynthesizer_inc,
  install: true,
  install_dir: gimpplugindir / 'plug-ins' / plugin_name,
)
