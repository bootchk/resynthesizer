# Generate and install i18n files.
# Inputs are .po files, from translators, readable text.
# Outputs are .mo files, binary, only machine readable.

# Resynthesizer is a group, or suite, of plugins.
# The usual naming for a .mo file for a group of plugins
# is like gimp30-script-fu.mo.
# "gimp30-script-fu" is in the role of a "domain name".
# And the plugin overrides its usual domain name
# (the name of the plugin)
# by calling set_i18n( ..., "gimp30-script-fu", ..).
# The plugin named "script-fu" does that,
# it is the server for all /script plugins.

# i18n is a module of meson.  gettext is a method of that module
# This would generate .mo for all LINGUAS and install them.
# But we don't use this, we have a special installation.
# i18n.gettext('resynthesizer3', preset: 'glib')
# That also seems to install to e.g. /usr/local/share/locale/es/LC_MESSAGES
# which is not where GIMP looks?

# We have one .po file covering all the plugins of the group.
# We generate one .mo file, and install it with each plugin, under a different name.
# In the place where the plugin machinery looks: 
# in the subdir "locale" of the plugin's dir,
# in a file foo.mo where foo is the same name as the plugin and it's dir.
# <GIMP install path>/plug-ins/<plugin>/locale/<language>/LC_MESSAGES/<plugin>.mo

# Alternatively, each script should point to the shared .mo
# by calling set_i18n() but that is not in SF yet.

# po/[es, fr, ..].po
#     msgfmt=>
# <GIMP install path> plug-ins/resynthesizer3/locale/[es, fr, ..]/LC_MESSAGES/resynthesizer3.mo
#     cp at install time =>
# <GIMP install path> plug-ins/[plugin-name, etc.]/local/[es, fr, ..]/LC_MESSAGES/plugin-name.mo


# Should match the .po file names
resynthesizer_i18n_languages = [
  'es',  # spanish
  'fr',
  'ru',
  'pt_BR',
  # TODO Turkish and Polish
]

# Remember what files we installed.
custom_install_files = []

foreach language_tag : resynthesizer_i18n_languages

  # target for a .mo e.g. es.mo and fr.mo
  # Note that we do install it, to .../plug-ins/resynthesizer3/locale/...
  # I think GIMP should ignore it as extraneous folder in /plug-ins.
  # Alternatively, build the artifact but don't install it.
  # We only install it because then we know where it is
  # unlike most built artifacts in the build dir.

  # a_language_target mutates in this for loop
  a_language_target = custom_target(
      # target name unique
      'target-' + language_tag ,

      # input is e.g. es.po in the current source directory, a .po for the group
      input: language_tag + '.po',

      # output e.g. is  es.mo
      output: language_tag + '.mo', 

      command: ['msgfmt', '@INPUT@', '-o', '@OUTPUT@'], 
      install: true,
      install_dir: gimpplugindir / 'resynthesizer3' / 'locale' / language_tag / 'LC_MESSAGES')

  # Copy and rename the shared .mo file to each of a group of plugin's locale directories
  foreach plugin_name : resynthesizer_plugin_names

    # Variables holding from and to paths for copy at install time.
    # The paths are platform portable.
    
    # The from file was previously installed by meson.  Copy from the installed file.
    # full path to installed file is gimpplugindir / 'resynthesizer3' / 'locale' / 'es' / 'LC_MESSAGES' / 'resynthesizer3.mo'
    from_file = a_language_target.full_path()

    to_dir = gimpplugindir / plugin_name / 'locale' / language_tag / 'LC_MESSAGES'
    to_file = to_dir / plugin_name + '.mo'


    # Copy an installed resynthesizer3.mo to a specific plugin's directory, renaming it to match the plugin name.
    # cp does not have option to create destination dirs, so create directory first.
    # The @0@ are placeholders for a format string.
    meson.add_install_script(
      'sh', '-c',
      'mkdir -p "@0@" && cp -f "@1@" "@2@"'.format(to_dir, from_file, to_file ))

    custom_install_files += to_file
  endforeach
endforeach


summary('Installing i18n for plugins:', resynthesizer_plugin_names)
summary('Installing i18n for languages:', resynthesizer_i18n_languages)
summary('Installing custom i18n files:', custom_install_files)