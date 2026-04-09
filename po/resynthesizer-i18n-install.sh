#!/bin/sh

# This is a custom install script 
# for the i18n data of the resynthesizer suite of plugins for GIMP.
# It is run by Meson during the installation phase,
# and is responsible for installing the compiled .mo files for translations
# to the appropriate directories where GIMP can find them at runtime.

# Requires the 'msgfmt' tool from GNU gettext to compile .po files into .mo files.
if ! command -v msgfmt >/dev/null 2>&1
then
  echo "msgfmt from GNU gettext is not available."
  return 1
fi

# The arguments passed to this script are:
# 1. LIBDIR: The destination directory for installation, platform-specific, 
# e.g., /usr/local/lib or /usr/lib 
# or some other temporary directory when using DESTDIR for subsequent packaging step.
# It comes from the Meson option 'libdir', 
# and Meson will automatically adjust it for the platform.

# This script also uses environment variables set by Meson 
# --MESON_INSTALL_DESTDIR_PREFIX-- the combined path of DESTDIR and MESON_INSTALL.
# You should not use DESTDIR directly in the script, but you can use MESON_INSTALL_DESTDIR_PREFIX.

echo ">>>>>>>>>>>Running custom install script for resynthesizer suite."
echo "Destination: ${MESON_INSTALL_DESTDIR_PREFIX}"
echo "Arg LIBDIR: $@"

# i18n data is installed as subdirectory of a plugin's root directory.
# This makes the installation "relocatable" 
# i.e. it is "bundled" with the plugin, and can be moved around with the plugin.
# Really should be a concern of packagers, and the old way-- installing
# to the platform's standard locale directory -- IMO should be supported by GIMP.
# The packagers should be responsible for bundling/sandboxing the plugins and their translations.
# For example, flatpak/snap/appImage should redirect a plugin's access to the standard locale directory
# to the bundled locale directory.

# Where GIMP installs system-wide plugins.
GimpPluginDir="${MESON_INSTALL_DESTDIR_PREFIX}/$1/gimp/3.0/plug-ins"
echo "GimpPluginDir: ${GimpPluginDir}"


# POSIX equivalent of an array (sort of...)
# which is used to define the components which have translations
#
# The "engine" plugin is the "resynthesizer" plugin, 
# which is the core/helper/engine of the suite,
# and has translations for error messages.
#
# The "controls" plugin is the plugin that provides a GUI 
# for the raw/bare/not-nested engine plugin
set -- "resynthesizer" \
       "plug-in-heal-selection" \
       "plug-in-heal-transparency" \
       "plug-in-uncrop" \
       "plug-in-map-style" \
       "plug-in-render-texture" \
       "plug-in-resynth-fill-pattern" \
       "plug-in-resynth-controls"

# Get available translations directly from the source folder
# Alternatively LINGUAS could be used for that
for lang in $(ls -1 ${MESON_SOURCE_ROOT}/po/*.po)
do
  LangBase=$(basename $lang .po)
  echo "Installing $LangBase translations"
  # Compile one languages .po file into a .mo file using msgfmt.
  # The input is in the source directory,
  # and the output is a temporary .mo file in the current build directory,
  # which we will copy to the final destination in the following loop.
  # The input is named for a language, e.g., 'cs' for Czech,
  # and the output is named 'resynthesizer3.mo' which is a "domain" name
  # shared by the suite of plugins, and is the name that matches the domain name in the source code,
  # that gettext uses to find the translations at runtime.
  msgfmt -o resynthesizer3.mo ${MESON_SOURCE_ROOT}/po/$LangBase.po

  # Do the placement work for the listed plugins
  for plugin in $@;
  do
    # Make the destination directory if it doesn't exist (-p)
    # The destination directory is the destination for one languages .mo file, for one plugin.
    #     where GIMP installs plugins:
    #        libdir/
    #        gimp/3.0/plug-ins/
    #     a particular plugin root dir: resynthesizer/
    #     where gettext looks for translations for a language: locale/<language code>/LC_MESSAGES
    mkdir -p $GimpPluginDir/$plugin/locale/$LangBase/LC_MESSAGES

    # Now we copy it to the final destination for the respective language
    cp resynthesizer3.mo $GimpPluginDir/$plugin/locale/$LangBase/LC_MESSAGES
  done
done

echo "Finished installing translations for resynthesizer suite."
