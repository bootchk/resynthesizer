

#include <glib/gprintf.h>

#include "debug.h"


void
debug(const char * message)
{
  g_message("%s", message);
}
