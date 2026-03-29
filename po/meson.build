# Generate and install i18n files.
# Inputs are .po files, from translators, readable text.
# Outputs are .mo files, binary, only machine readable.

# Resynthesizer is a group, or suite, of plugins.
# But it does not install i18n data like for other groups of plugins.
# Because GIMP does not support that, for third party groups of plugins.
# GIMP only supports that for its own groups of plugins.

# How groups of plugins usually install i18n
#
# The usual naming for a .mo file for a group of plugins
# is like gimp30-script-fu.mo.
# "gimp30-script-fu" is in the role of a "domain name".
# And the plugin overrides its usual domain name
# (the name of the plugin)
# by calling set_i18n( ..., "gimp30-script-fu", ..).
# The plugin named "script-fu" does that,
# it is the server for all /script plugins.





# i18n is a module of meson.  gettext is a method of that module
# This generates a .mo for each LINGUAS e.g. for es
# in one file "resynthesizer3.mo"
# installed to e.g. /usr/local/share/locale/es/LC_MESSAGES.
# Later, we fan this out.
# Not actually used at runtime from its installed location.
# We could delete it after the build, but we don't.
i18n.gettext('resynthesizer3', preset: 'glib')



# Should match the .po file names
# Later we iterate over this.
resynthesizer_i18n_languages = [
  'cs',  # Czech
  'es',  # spanish
  'fr',
  'ru',
  'pt_BR', # Brazilian Portugese
  # TODO Turkish and Polish, need to cherrypick from master branch
]


# As discussed above, this does not install one catalog (directory of 18n .mo files)
# for the group of resynthesizer plugins,
# but instead installs the same catalog with different domain names,
# in many places (in a locale subdirectory of each plugin.)
# I.E. copy, rename, and fan out shared group resynthesizer.mo file to each plugin's locale directory
# 
# This seems clunky, wasteful, and fragile.
# But probably the best we can do given how GIMP does i18n.


foreach language_tag : resynthesizer_i18n_languages

  # for each outer plugin filename and also the engine plugin filename 'resynthesizer'
  foreach plugin_name : resynthesizer_plugin_names + 'resynthesizer'
    
    # The source of the copy was previously installed by meson to the platform's shared locale dir.
    # We copy from the installed file.
    # Typical full path to source_file is /usr/local/share/locale/es/LC_MESSAGES/resynthesizer3.mo
    #                                     prefix    localedir    language_tag    group name

    # get_option('localedir') == 'share/locale' and MUST be prefixed
    source_file = get_option('prefix') / get_option('localedir') / language_tag / 'LC_MESSAGES' / 'resynthesizer3.mo'

    # When copying, distribute or fan out the group file
    # to a subdirectory "locale/" under each plugin's root directory
    # in GIMP's directory of all plugins "GIMP/plug-ins/"
    dest_dir = gimpplugindir / plugin_name / 'locale' / language_tag / 'LC_MESSAGES'

    # When copying, rename the shared file "resynthesizer3.mo" to match the file name of the plugin, the so-called "domain"
    # Each plugin declares a domain the same as the file name of the plugin.
    dest_file = dest_dir / plugin_name + '.mo'

    # Copy the shared .mo to a specific plugin's directory, renaming it to match the plugin name.
    # cp does not have option to create destination dirs, so create directory first.
    meson.add_install_script(
      'sh', '-c',
      'mkdir -p "@0@" && cp -f "@1@" "@2@"'.format(dest_dir, source_file, dest_file ))

  endforeach
endforeach

# !!! The i18n data installed to platform locale directory not actually used
# and could be deleted.

summary('Installing i18n for plugins:', resynthesizer_plugin_names + 'resynthesizer')
summary('Installing i18n for languages:', resynthesizer_i18n_languages)