project('resynthesizer',
  'c',
  version: '3.0.0',
  meson_version: '>=0.50.0',
  default_options: [
    'cpp_std=gnu++14',
    'buildtype=debugoptimized',
  ],
)

# Define meson var for libresynthesizer headers
# TODO move to a separate include dir
resynthesizer_inc = include_directories('lib')


# Is the order important to meson?
# Anyway, this is the order of dependency

# Headers for engine library
# subdir('include')

# engine library
subdir('lib')

# engine plugin
subdir('src')

# TODO Python plugins
#subdir('PluginScripts')

# TODO translations
#subdir('po')

# TODO help
# The help is still for the plugin template, not resynthesizer
