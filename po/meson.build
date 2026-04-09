# Generate and install i18n data files.
# Inputs are .po files, from translators, readable text.
# Outputs are .mo files, binary, only machine readable.

# Resynthesizer is a group, or suite, of plugins.
# But it does not install i18n data like for other groups of plugins
# to the platform's standard locale directory.
# Because GIMP does not support that, for third party groups of plugins.
# GIMP only supports that for its own groups of plugins.

# How GIMP installs i18n for its own groups of plugins i18n
#
# The usual naming for a .mo file for a group of plugins
# is like gimp30-script-fu.mo.
# "gimp30-script-fu" is in the role of a "domain name".
# And the plugin overrides its usual domain name
# (the name of the plugin)
# by calling set_i18n( ..., "gimp30-script-fu", ..).
# The plugin named "script-fu" does that, to serve all /script plugins.

# Again, GIMP does not support such an installation for
# third-party groups of plugins.
# Instead it requires the i18n data be bundled
# in a subdirectory of the plugin's root directory.
# So the plugin's entirety is relocatable by one directory move.


# This generates a .mo for each LINGUAS e.g. for es
# in a file "resynthesizer3.mo"
# installed to e.g. /usr/local/share/locale/es/LC_MESSAGES.
# i18n is a module of meson.  gettext is a method of that module.
# This is the old-school installation, much simpler.
# NOT USED: i18n.gettext('resynthesizer3', preset: 'glib')

# Later, we could copy/fan that out.
# Again, GIMP does not let a plugin use i18n data
# from the platform's (Linux) standard locale directory.




# Should match the .po file names
# NOT USED.
resynthesizer_i18n_languages = [
  'cs',  # Czech
  'es',  # spanish
  'fr',
  'pl',  # polish
  'ru',
  'pt_BR', # Brazilian Portugese
  'tr',  # turkish
]


# As discussed above, this does not install a single catalog (directory of 18n .mo files)
# to be shared by the group of resynthesizer plugins,
# but instead installs the same catalog in many places (in a locale subdirectory of each plugin.)
# I.E. copy and fan out shared group resynthesizer.mo file to each plugin's locale directory
# 
# This seems clunky, wasteful, and fragile.
# But probably the best we can do given how GIMP does i18n.

# Invoke custom install script passing LIBDIR
meson.add_install_script('resynthesizer-i18n-install.sh', get_option('libdir'))

summary('Installing i18n for plugins:', resynthesizer_plugin_names + 'resynthesizer')
summary('Installing i18n for languages:', resynthesizer_i18n_languages)
