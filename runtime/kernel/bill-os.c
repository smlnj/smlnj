#include "bill-os.h"
#include <stdlib.h>

#if defined(__CYGWIN__) || defined(__MINGW32__)

struct netent * getnetbyname(const char * name)
{
   return NULL;
}

struct netent * getnetbyaddr(long net, int type)
{
   return NULL;
}

#endif
