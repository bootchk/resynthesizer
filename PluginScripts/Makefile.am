## Process this file with automake to produce Makefile.in

## lkk 2011 I hacked this file from gimp../plugins/pygimp/plugins/Makefile.am
## pluginexecdir tells autotools where to install scripts
## scripts tells autotools the list of installable Python language scripts

pluginexecdir = $(gimpplugindir)/scripts

scripts = \
	plugin-heal-selection.scm      \
	plugin-resynth-sharpen.scm     \
	plugin-resynth-enlarge.scm     \
	plugin-uncrop.scm              \
	plugin-render-texture.scm      \
	plugin-map-style.scm           \
	plugin-heal-transparency.scm   \
	plugin-resynth-fill-pattern.scm

pluginexec_SCRIPTS = $(scripts)

EXTRA_DIST = $(scripts)
