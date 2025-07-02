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

mac_gimp_app = get_option('mac-gimp-app')
if mac_gimp_app == ''
  # libgimp, the API to gimp.
  # Need headers i.e. include file libgimp.h
  # If fails, try install a dev package such as "gimp-3.0-dev"
  # version 2.99 is the beta for v3.0
  gimp_dep = dependency('gimp-3.0', version : '>=2.99.0')
  gimp_rpath = {}
else
  # On macOS, we'll grab the shared libraries from the GIMP App.
  gimp_inc_dir = get_option('gimp-source-dir')
  if gimp_inc_dir == ''
    error('Missing reqired option for macOS: gimp-source_dir')
  endif

  mac_rpath = mac_gimp_app / 'Contents/Resources'
  mac_dylib_path = mac_rpath / 'lib'

  # The dependencies of libgimp-3.0, either Homebrew or MacPorts should work.
  # These are only needed as header file dependencies of <libgimp/gimp.h>.
  _babl_includes = dependency('babl-0.1').partial_dependency(compile_args: true)
  _gegl_includes = dependency('gegl-0.4').partial_dependency(compile_args: true)
  _cairo_includes = dependency('cairo').partial_dependency(compile_args: true)
  _gexiv_includes = dependency('gexiv2').partial_dependency(compile_args: true)
  _pango_includes = dependency('pango').partial_dependency(compile_args: true)
  _pixbuf_includes = dependency('gdk-pixbuf-2.0').partial_dependency(compile_args: true)

  gimp_inc = include_directories(gimp_inc_dir)
  gimp_dep = declare_dependency(
     include_directories: [
       gimp_inc,
     ],
     dependencies: [
       cc.find_library(
         'gimp-3.0',
         dirs : [mac_dylib_path],
         has_headers : ['libgimp/gimp.h', 'libgimpbase/gimpversion.h'],
         header_include_directories : [gimp_inc],
         required: true),

       # The following libraries from GIMP.app must be mentioned explicitly
       # to the linker, which can't follow transitive dependencies just by
       # looking at libgimp-3.0.dylib.
       cc.find_library('glib-2.0',    dirs : [mac_dylib_path], required: true),
       cc.find_library('gobject-2.0', dirs : [mac_dylib_path], required: true),
       cc.find_library('gegl-0.4',    dirs : [mac_dylib_path], required: true),
       cc.find_library('babl-0.1',    dirs : [mac_dylib_path], required: true),
       cc.find_library('intl',        dirs : [mac_dylib_path], required: true),

       # The include paths.
       _babl_includes,
       _gegl_includes,
       _cairo_includes,
       _gexiv_includes,
       _pango_includes,
       _pixbuf_includes,
     ],
  )
  gimp_rpath = {'install_rpath': mac_rpath}
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
  dependencies: [
    gimp_dep,
  ],
  c_args: [ '-DG_LOG_DOMAIN="Resynthesizer"', '-DGIMP_COMPILATION', ],
  link_with : [
    resynth_lib,
  ],
  include_directories: resynthesizer_inc,
  install: true,
  install_dir: gimpplugindir / plugin_name,
  kwargs: gimp_rpath,
)
