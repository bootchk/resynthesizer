#ifndef __PLUGIN_INTL_H__
#define __PLUGIN_INTL_H__


//#ifndef GETTEXT_PACKAGE
//#error "config.h must be included prior to plugin-intl.h"
//#endif

#include <libintl.h>

#define _(String) gettext (String)

#ifdef gettext_noop
#    define N_(String) gettext_noop (String)
#else
#    define N_(String) (String)
#endif


#endif /* __PLUGIN_INTL_H__ */
