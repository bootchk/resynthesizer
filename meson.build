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
# gettext_package= 'resynthesizer3')


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
# but are installed to the /plugins dir
# and executed by the standalone SF interpreter
# (instead of extension-script-fu)

# TODO this is platform specific, for Ubuntu
# TODO should install to user specific location (customizations) ???
# define the location to install gimp plugins.
gimpplugindir = prefix / 'lib/x86_64-linux-gnu/gimp/2.99/plug-ins'



# Is the order important to meson?
# Anyway, this is the order of dependency

# engine library
subdir('lib')

subdir('enginePlugin')
subdir('outerPlugins')
subdir('test')

# translations
subdir('po')
