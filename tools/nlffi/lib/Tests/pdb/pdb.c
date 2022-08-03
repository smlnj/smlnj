# include <stdio.h>
# include <stdlib.h>
# include <string.h>

typedef struct entry *entryptr;
typedef struct forward *forwardptr;

extern entryptr getnext (forwardptr);
extern void setnext (forwardptr, entryptr);
extern forwardptr forwardalloc (void);

struct entry {
  char *last;
  char *first;
  unsigned foo : 2;
  unsigned     : 5;
  signed bar   : 3;
  short age;
  float weight;
  FILE *f;
  int (*baz) (float, int);
  forwardptr fwd;
};

#define EOFF0(x,p) ((char *)&((p)->x)-(char *)p)
#define EOFF(x) EOFF0(x,(struct entry *)0)

extern int print (struct entry *);
extern int ppp (double, struct entry, float);

extern struct entry *db;

#define MAX 512

struct entry *db = NULL;

static void prompt (const char *p)
{
  printf ("%s: ", p);
}

static char *asks (const char *p)
{
  char buf[MAX];
  int len;
  char *res;

  prompt (p);
  fgets (buf, MAX, stdin);
  len = strlen (buf);
  if ((len > 0) && (buf[len-1] == '\n')) buf[--len] = '\0';
  if (len == 0) return NULL;
  res = malloc (len + 1);
  strcpy (res, buf);
  return res;
}

static int aski (const char *p)
{
  char *s = asks (p);
  return s ? atoi(s) : 0;
}

static double askd (const char *p)
{
  char *s = asks (p);
  return s ? strtod(s, NULL) : 0.0;
}

static int baz (float a, int b)
{
  static int c = 0;
  printf ("baz (%g, %d) -> %d\n", (double)a, b, c);
  return c++;
}

void _init (void)
{
  char *last;

  printf (
      "Offsets: last@%d, first@%d, age@%d, weight@%d, f@%d, baz@%d, fwd@%d\n",
      EOFF(last), EOFF(first), EOFF(age), EOFF(weight), EOFF(f), EOFF(baz),
      EOFF(fwd));

  while ((last = asks ("Last name")) != NULL) {
    struct entry *n = malloc (sizeof (struct entry));
    n->fwd = forwardalloc ();
    setnext (n->fwd, db);
    n->f = stdin;
    db = n;
    n->last = last;
    n->first = asks ("First name");
    n->age = aski ("Age");
    n->foo = n->age & 0x3;
    n->bar = (n->age >> 2) & 0x7;
    n->weight = askd ("Weight");
    n->baz = baz;
  }
}

int print (struct entry *l)
{
  int i;
  for (i = 0 ; l != NULL; l = getnext (l->fwd), ++i)
    printf ("Last: %s\nFirst: %s\nFoo: %u\nBar: %d\nAge: %d\nWeight: %.2f\n",
	    l->last, l->first, (int) l->foo, (int) l->bar,
	    (int) l->age, (double) l->weight);
  return i;
}

int ppp (double f, struct entry e, float g)
{
  printf ("This is ppp(%g, ..., %g)\n", f, (double)g);
  return print (&e);
}

int pppp (float f, struct entry e, double g)
{
  printf ("This is ppp(%g, ..., %g)\n", (double)f, g);
  return print (&e);
}

FILE* fileof (struct entry *e) { return e->f; }
