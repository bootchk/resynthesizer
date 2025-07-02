# version 2 also compiled a C-language plugin: resynthesizer-gui
# Since 2023 replaced by a ScriptFu plugin.
# Since 2023 we obsolete the C-language control panel plugin because
# 1) uses obsolete Gtk-2
# 2) is non-declarative


plugin_name = 'resynthesizer'
# assert defined earlier: gimpplugindir = '/usr/local/lib/x86_64-linux-gnu/gimp/2.99'

# !!! Other .h files have compilable code (they are not just headers)
# but we only need to list the top compiled files.

resynthesizer_sources = [
  'plugin.c',     # main for a plugin
  'resynthesizer.c',
  'debug.c',
  'pluginParams.c',
  'drawable.c',
  'format.c',
  'byte_sequence.c',
  'animate.c',
  'adaptGimp.c',
]


# define dependencies on libraries

# On macOS, we'll grab the shared libraries from the GIMP App.
mac_gimp_app = get_option('mac-gimp-app')
if mac_gimp_app != ''
  mac_rpath = mac_gimp_app / 'Contents/Resources'
  mac_dylib_path = mac_rpath / 'lib'
  mac_inc_dir = get_option('mac-gimp-source')
  if mac_inc_dir == ''
    error('Missing reqired option for macOS: mac-gimp-source')
  endif

  # The dependencies of libgimp-3.0, either Homebrew or MacPorts should work.
  # These are only needed as header file dependencies of libgimp. Sadly meson
  # doesn't provide a way to extract just the include paths from a dependency
  # object.
  _mac_babl_headers = dependency('babl-0.1')
  _mac_gegl_headers = dependency('gegl-0.4')
  _mac_cairo_headers = dependency('cairo')
  _mac_gexiv_headers = dependency('gexiv2')
  _mac_pango_headers = dependency('pango')
  _mac_pixbuf_headers = dependency('gdk-pixbuf-2.0')

  gimp_rpath = {'install_rpath': mac_rpath}
  gimp_inc = [include_directories(mac_inc_dir)]
  gimp_dep = [
    cc.find_library(
      'gimp-3.0',
      dirs : [mac_dylib_path],
      has_headers : ['libgimp/gimp.h', 'libgimpbase/gimpversion.h'],
      header_include_directories : gimp_inc,
      required: true),
    cc.find_library(
      'glib-2.0',
      dirs : [mac_dylib_path],
      required: true),
    cc.find_library(
      'gobject-2.0',
      dirs : [mac_dylib_path],
      required: true),
    cc.find_library(
      'gegl-0.4',
      dirs : [mac_dylib_path],
      required: true),
    cc.find_library(
      'babl-0.1',
      dirs : [mac_dylib_path],
      required: true),
    cc.find_library(
      'intl',
      dirs : [mac_dylib_path],
      required: true),

    _mac_babl_headers,
    _mac_gegl_headers,
    _mac_cairo_headers,
    _mac_gexiv_headers,
    _mac_pango_headers,
    _mac_pixbuf_headers,
  ]
else
  # libgimp, the API to gimp.
  # Need headers i.e. include file libgimp.h
  # If fails, try install a dev package such as "gimp-3.0-dev"
  # version 2.99 is the beta for v3.0
  gimp_dep = [dependency('gimp-3.0', version : '>=2.99.0')]
  gimp_inc = []
  gimp_rpath = {}
endif

# resynthesizer engine plugin calls (depends on)
# the resynthesizer engine library (libresynthesizer).
# Which we also first build.
# That dependency on libresynthesizer is declared below, see links_with


# Since 3.0 a plugin must install to a dir named the same as the plugin
# Hence: install_dir: ...

# Summarizing dependency: libgimp and libresynthesizer

executable(plugin_name,
  resynthesizer_sources,
  dependencies: gimp_dep,
  c_args: [ '-DG_LOG_DOMAIN="Resynthesizer"', '-DGIMP_COMPILATION', ],
  link_with : [
    resynth_lib,
  ],
  include_directories: [resynthesizer_inc] + gimp_inc,
  install: true,
  install_dir: gimpplugindir / plugin_name,
  kwargs: gimp_rpath,
)
