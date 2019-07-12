## Process this file with automake to produce Makefile.in

## lkk 2011 I hacked this file from gimp../plugins/pygimp/plugins/Makefile.am
## pluginexecdir tells autotools where to install scripts
## scripts tells autotools the list of installable Python language scripts

pluginexecdir = $(gimpplugindir)/plug-ins

scripts = \
	plugin-heal-selection.py	\
	plugin-resynth-sharpen.py	\
	plugin-resynth-enlarge.py	\
	plugin-uncrop.py	\
	plugin-render-texture.py	\
	plugin-map-style.py	\
	plugin-heal-transparency.py \
	plugin-resynth-fill-pattern.py \
  plugin-resynthesizer-gui.py \
  plugin-false-color-match.py
	

pluginexec_SCRIPTS = $(scripts)

EXTRA_DIST = $(scripts)
