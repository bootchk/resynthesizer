project('resynthesizer',
  'c',
  version: '3.0.0',
  meson_version: '>=0.53.0',
  default_options: [
    'cpp_std=gnu++14',
    'buildtype=debugoptimized',
  ],
)

# Get Meson module for translation
i18n = import('i18n')
# unique name of gettext package
# gettext_package = 'resynthesizer3'

###### List names

# These are the outer plugin names.
# Same name for files and names of the plugin's PDB procedure
# and names of the run funcs in Scheme.
# Outer plugins appear in GIMP menus.
resynthesizer_plugin_names = [
  'plug-in-heal-selection',
  'plug-in-heal-transparency',
  'plug-in-resynth-controls',
  'plug-in-map-style',
  'plug-in-render-texture',
  'plug-in-uncrop',
  'plug-in-resynth-fill-pattern',
]

# There is one other plugin: the engine, in C.
# It is in the PDB, but not in GIMP menus.

# Not ported yet, so not installed.
# As ported, move to the list above.
unported_plugin_basenames = [
  'plug-in-resynth-enlarge',
  'plug-in-resynth-sharpen',
]

###### Configuration

# Configuration statements were copied from gimp/meson.build
# They are more or less canonical

# create a meson configuration object
conf = configuration_data()

# Get build options into the configuration

conf.set('SYNTH_THREADED',          get_option('synth-threaded'))
conf.set('SYNTH_USE_GLIB',          get_option('synth-use-glib'))

# if we are using glib
if get_option('synth-use-glib')
  # we can still choose to use posix threads for threading
  conf.set('SYNTH_USE_GLIB_THREADS',  get_option('synth-use-glib-threads'))
else
  # otherwise we MUST use posix threads
  conf.set('SYNTH_USE_GLIB_THREADS',  false)
endif

# TODO depends on GLib?
conf.set('SYNTH_DEBUG',             get_option('synth-debug'))
conf.set('SYNTH_ANIMATE',           get_option('synth-animate'))
conf.set('SYNTH_DEEP_PROGRESS',     get_option('synth-deep-progress'))

# generate config.h from the conf
# config.h is only included by the library, only affects it.
configure_file(
  output: 'resynth-config.h',
  configuration: conf
)
#compiler_args +='-DHAVE_CONFIG_H'




prefix = get_option('prefix')

# We don't install libresynthesizer, only static link to it.

# Define meson var for libresynthesizer headers
# Referenced both by lib, src/resynthesizer, and test
resynthesizer_inc = include_directories('lib')


# All built and installed artifacts are Gimp plugins.
# The engine plugin is a C binary.
# The others are ScriptFu Scheme scripts.

# Since Gimp v3, the plugins that are ScriptFu scripts
# are NOT installed to a different directory /scripts
# but are installed to the /plug-ins dir
# and executed by the standalone SF interpreter
# (instead of extension-script-fu)

# TODO this is platform specific, for Ubuntu
# TODO should install to user specific location (customizations) ???
# define the location to install gimp plugins.
gimpplugindir = get_option('gimp-plugin-dir')
if gimpplugindir == ''
  gimpplugindir = prefix / get_option('libdir') / 'gimp/3.0/plug-ins'
endif



# Is the order important to meson?
# Anyway, this is the order of dependency

# engine library
subdir('lib')

subdir('enginePlugin')
subdir('outerPlugins')

if get_option('install-test')
  subdir('test')
endif

# i18n translations
if get_option('install-translations')
  subdir('po')
endif
