# include <stdlib.h>

typedef struct entry *entryptr;
typedef struct forward *forwardptr;

struct forward {
  entryptr next;
};

entryptr getnext (forwardptr f)
{
  return f->next;
}

void setnext (forwardptr f, entryptr n)
{
  f->next = n;
}

forwardptr forwardalloc (void)
{
  return malloc (sizeof (struct forward));
}
