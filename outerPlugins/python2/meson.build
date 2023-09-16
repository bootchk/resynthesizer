
# Install resynthesizer's outer plugins

# This does not check that python is installed.
# If not, the installed python plugins won't work.
# Gimp 2 python plugins (GimpFu) use only Python2
# Some distros do not install Python2
# Some distros do not build the Python2 packaged by gimp.org

# plugin_basenames is defined one level up

# Note: Gimp installs its own Python plugins without the 'plugin-' suffix
# Others use prefix python-fu-, to denote they are GimpFu plugins versus Python PyGObject plugins
# IT is not good to label filenames with too much type information.

# install_data() takes a list of names
# but we must iterate over install_data because, since Gimp 3.0,
# each plugin must be installed to a directory of the same name as basename

# Install source file, with executable permissions.
# TODO for v3 only:
# Nested in directory of the same basename
# TODO this should be for v2

foreach basename : plugin_basenames
  src_file_name = basename + '.py'
  install_data(src_file_name,
               install_dir: gimpplugindir / 'plug-ins' / basename,
               install_mode: 'rwxr-xr-x')
endforeach

summary('Installed plugin names', plugin_basenames)
