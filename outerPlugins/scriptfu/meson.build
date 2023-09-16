# Install resynthesizer's outer plugins

# plugin_basenames is defined one level up

# install_data() takes a list of names
# but we must iterate over plugins because, since Gimp 3.0,
# each plugin must be installed to a directory of the same name as basename

# Install source file, with executable permissions.

# !!! Added a shebang to each script (beyond original by author itr-tert)
# Interpreted by standalone interpreter, not extension-script-fu

foreach basename : plugin_basenames
  src_file_name = basename + '.scm'
  install_data(src_file_name,
               install_dir: gimpplugindir / 'plug-ins' / basename,
               install_mode: 'rwxr-xr-x')
endforeach

summary('Installed plugin names', plugin_basenames)
