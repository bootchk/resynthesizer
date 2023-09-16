project('resynthesizer',
  'c',
  version: '3.0.0',
  meson_version: '>=0.53.0',
  default_options: [
    'cpp_std=gnu++14',
    'buildtype=debugoptimized',
  ],
)

#generate config.h
#conf_data = configuration_data()
#configure_file(input : 'config.h.in',
#               output : 'config.h',
#               configuration : conf_data)

prefix = get_option('prefix')

# Define meson var for libresynthesizer headers
# TODO move to a separate include dir
resynthesizer_inc = include_directories('lib')

# TODO this is platform specific, for Ubuntu
# TODO should install to user specific location (customizations) ???
# define the location to install gimp plugins
gimpplugindir = prefix / 'lib/x86_64-linux-gnu/gimp/2.99'


# Is the order important to meson?
# Anyway, this is the order of dependency

# engine library
subdir('lib')

# engine plugin
subdir('src')

# plugins
subdir('outerPlugins')

# TODO translations
#subdir('po')
