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


#generate config.h
#conf_data = configuration_data()
#configure_file(input : 'config.h.in',
#               output : 'config.h',
#               configuration : conf_data)

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
