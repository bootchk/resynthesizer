

# Outer plugin basenames are same whether Python or scriptfu
# Only the suffix differs

plugin_basenames = [
  'plugin-heal-selection',
  'plugin-heal-transparency',
#  'plugin-map-style',
#  'plugin-render-texture',
#  'plugin-resynth-enlarge',
#  'plugin-resynth-fill-pattern',
#  'plugin-resynth-sharpen',
#  'plugin-uncrop',
#  'plugin-resynth-controls',
]

# v2 uses GimpFu, Python 2 plugins
if get_option('for-gimp-3')
  subdir('scriptfu')
else
  subdir('python2')
endif