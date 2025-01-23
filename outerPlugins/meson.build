
# Install resynthesizer's outer plugins

# Outer plugin basenames are same whether Python or scriptfu
# Only the suffix differs

plugin_basenames = [
  'plugin-heal-selection',
  'plugin-heal-transparency',
    'plugin-resynth-controls',
]

# Not ported yet, so not installed.
# As ported, move to the list above.
unported_plugin_basenames = [
  'plugin-map-style',
  'plugin-render-texture',
  'plugin-resynth-enlarge',
  'plugin-resynth-fill-pattern',
  'plugin-resynth-sharpen',
  'plugin-uncrop',
]


# install_data() takes a list of names
# but we must iterate over plugins because, since Gimp 3.0,
# each plugin must be installed to a directory of the same name as basename

# Install source file, with executable permissions.

# !!! Added a shebang to each script (beyond original by author itr-tert)
# Interpreted by standalone interpreter, not extension-script-fu

# Usual dir is .../GIMP/3.0/plug-ins

foreach basename : plugin_basenames
  src_file_name = basename + '.scm'
  install_data(src_file_name,
               install_dir: gimpplugindir / basename,
               install_mode: 'rwxr-xr-x')
endforeach

summary('Installed plugin names', plugin_basenames)
