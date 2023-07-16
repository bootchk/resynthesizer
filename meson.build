project('resynthesizer',
  'c',
  version: '3.0.0',
  meson_version: '>=0.53.0',
  default_options: [
    'cpp_std=gnu++14',
    'buildtype=debugoptimized',
  ],
)

# Define meson var for libresynthesizer headers
# TODO move to a separate include dir
resynthesizer_inc = include_directories('lib')

# TODO this is platform specific, for Ubuntu
# TODO should install to user specific location (customizations) ???
# define the location to install gimp plugins
gimpplugindir = '/usr/local/lib/x86_64-linux-gnu/gimp/2.99'


# Is the order important to meson?
# Anyway, this is the order of dependency

# Headers for engine library
# subdir('include')

# engine library
subdir('lib')

# engine plugin
subdir('src')

# Python plugins
subdir('PluginScripts')

# TODO translations
#subdir('po')

# TODO help
# The help is still for the plugin template, not resynthesizer
