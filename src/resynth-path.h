
#include <glib.h>

#define locale_rel_path "/resynthesizer-locale"
// There is no basis for this size.
#define path_max_size 260*4

#if defined(G_OS_WIN32)
#  include <windows.h>

char * get_excutable_path () {

     static char path[path_max_size] = {0};
     int path_bytes = sizeof (path);

     // `MAX_PATH` is traditionally 260.
     // `exteneded length path` length is 32767. too long.

     if (0 == GetModuleFileName (NULL, path, path_bytes)) {
	  return "";
     };
     return path;
}

#else

#  include <unistd.h>

char * get_excutable_path () {
     static char path[path_max_size] = {0};
     int path_bytes = sizeof(path);

     if (-1 == readlink ("/proc/self/exe", path, path_bytes - 1)) {
	  return "";
     };
     return path;
}

#endif

char * get_resynthesizer_locale_dir () {
     static char path[path_max_size] = {0};
     char *exe_path = get_excutable_path ();
     gchar *heap1 = g_path_get_dirname (exe_path);
     gchar *heap2 = g_strconcat (heap1, locale_rel_path, NULL);
     g_free (heap1);
     g_strlcpy (path, heap2, path_max_size);
     g_free (heap2);

     return path;
}
