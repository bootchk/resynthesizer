
# Install resynthesizer's outer plugins

# Outer plugin basenames are same whether Python or scriptfu
# Only the suffix differs



# install_data() takes a list of names.
# resynthesizer_plugin_names is defined in root meson.build
# Iterate over plugins because, since Gimp 3.0,
# each plugin must be installed to a directory of the same name as basename,
# and not in one directory /scripts as for scripts served by extension-script-fu.

# Install source file, with executable permissions.

# !!! Added a shebang to each script (beyond original by author itr-tert)
# Interpreted by standalone interpreter, not extension-script-fu

# Usual dir is .../GIMP/3.0/plug-ins

foreach basename : resynthesizer_plugin_names
  src_file_name = basename + '.scm'
  install_data(src_file_name,
               install_dir: gimpplugindir / basename,
               install_mode: 'rwxr-xr-x')
endforeach

summary('Installed plugin names', resynthesizer_plugin_names)
