

#include <glib/gprintf.h>

#include "debug.h"


void
debug(const char * message)
{
  g_printerr(message);
  g_printerr("\n");
}
