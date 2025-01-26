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

# i18n is a module of meson.
# gettext is a method of that module
# This would generate .mo and install them.
# But we don't use this, we have a special installation.
# i18n.gettext('resynthesizer3', preset: 'glib')

# We have one .po file covering all the plugins of the group.
# We generate one .mo file, and install it with each plugin, under a different name.
# In the place where the plugin machinery looks: 
# in the subdir "locale" of the plugin's dir,
# in a file foo.mo where foo is the same name as the plugin and it's dir.
# <GIMP install path>/plug-ins/<plugin>/locale/<language>/LC_MESSAGES/<plugin>.mo

# Build .mo translation files.
# For all languages in the LINGUAS file in this directory
# for all <language>.po in this directory.
# Using the domain name shared by the group of plugins: "resynthesizer3"
built_mo_files = i18n.gettext('resynthesizer3',
             preset: 'glib')   # standard set of preset args to gettext

# If you install:false, it doesn't seem to leave the files in the build dir.
# With install, it installs to /usr/local/share/locate/fr/LC_MESSAGES/resynthesizer.mo
# But leaves them in the build dir, where we can install them where we really want them.

# Now exists many files in the build tree named:
# <buildRoot>/po/<language>/LC_MESSAGES/resynthesizer3.mo

if built_mo_files.length() < 4
  warning ('Failed to generate .mo files.')
endif




# Rename the group's .mo specific to a plugin.
# And install in each plugin's directory where Plugin class looks for it.
#
# The group's .mo has translations for all plugins in the group.
# Install the same file in many locations, renamed in each location.

# Alternatively, each script should point to the shared .mo
# by calling set_i18n() but that is not in SF yet.

# Should match the .po file names
resynthesizer_i18n_languages = [
  'es',  # spanish
  'fr',
  'ru',
  'pt_BR',
]

# e.g. install resynthesizer3.mo from build dir
# to .../plug-ins/plugin_heal_selection/locale/es/LC_MESSAGES/plugin_heal_selection.mo
# That is where the GIMP Plugin class looks for the .mo

foreach plugin_name : resynthesizer_plugin_names
  
  foreach language_tag : resynthesizer_i18n_languages
  
    install_data(meson.build_root() / 'po' / language_tag / 'LC_MESSAGES' / 'resynthesizer3.mo',
             rename : plugin_name + '.mo',
             install_dir: gimpplugindir / plugin_name / 'locale' / language_tag / 'LC_MESSAGES')

    endforeach

endforeach

summary('Installing i18n for plugins:', resynthesizer_plugin_names)
summary('Installing i18n for languages:', resynthesizer_i18n_languages)