
# Install resynthesizer's Python plugins

# This does not check that python is installed.
# If not, the installed plugins won't work.


# Note: Gimp installs its own Python plugins without the 'plugin-' suffix
# Others use prefix python-fu-, to denote they are GimpFu plugins versus Python PyGObject plugins
# IT is not good to label filenames with too much type information.

plugin_basenames = [
  'plugin-heal-selection',
  'plugin-heal-transparency',
  'plugin-map-style',
  'plugin-render-texture',
  'plugin-resynth-enlarge',
  'plugin-resynth-fill-pattern',
  'plugin-resynth-sharpen',
  'plugin-uncrop',
  'plugin-resynth-controls',
]

# install_data() takes a list of names
# but we must iterate over install_data because, since Gimp 3.0,
# each plugin must be installed to a directory of the same name as basename

# Install source file, with executable permissions, one per directory

foreach basename : plugin_basenames
  src_file_name = basename + '.py'
  install_data(src_file_name,
               install_dir: gimpplugindir / 'plug-ins' / basename,
               install_mode: 'rwxr-xr-x')
endforeach

summary('Installed plugin names', plugin_basenames)
