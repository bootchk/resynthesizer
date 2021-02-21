
# This does not check that python is installed.
# If not, the installed plugins won't work.


# TODO Gimp installs its own Python plugins without the 'plugin-' suffix
# Maybe the names should be python-fu-, since they are GimpFu plugins
# and not Python PyGObject plugins
# OTH, not good to label filenames with too much type information

plugins = [
  { 'name': 'plugin-heal-selection' },
  { 'name': 'plugin-heal-transparency' },
  { 'name': 'plugin-map-style' },
  { 'name': 'plugin-render-texture' },
  { 'name': 'plugin-resynth-enlarge' },
  { 'name': 'plugin-resynth-fill-pattern' },
  { 'name': 'plugin-resynth-sharpen' },
  { 'name': 'plugin-uncrop' },
]
# FUTURE 'resynth-control-panel'



# I don't understand what 'srcs' means to meson

# Cat standard suffix, then install, with executable permissions

# Since Gimp 3.0, must be installed to a directory of the same name as basename

foreach plugin : plugins
  name = plugin.get('name')
  srcs = plugin.get('srcs', name + '.py')

  install_data(srcs,
               install_dir: gimpplugindir / 'plug-ins' / name,
               install_mode: 'rwxr-xr-x')
endforeach
