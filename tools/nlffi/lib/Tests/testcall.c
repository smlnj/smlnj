#include <stdio.h>
#include <stdlib.h>

int tc1 (int i)
{ int r = 2 * i + 1;
  printf ("tc1 called with %d, returning %d\n", i, r);
  return r;
}

int tc2 (int i, int j)
{ int r = i + j - 1;
  printf ("tc2 called with %d and %d, returning %d\n", i, j, r);
  return r;
}

double tc3 (int i, double f)
{
  double r = f * i;
  printf ("tc3 called with %d and %g, returning %g\n", i, f, r);
  return r;
}

double tc4 (double f, int i)
{
  double r = f / i;
  printf ("tc4 called with %g and %d, returning %g\n", f, i, r);
  return r;
}

void dummy (void)
{
  return;
}

void click (void)
{
  printf ("click!\n");
  return;
}

void show (int i)
{
  printf ("showing %d\n", i);
  return;
}

int gen (void)
{
  static int i = 0;
  return i++;
}

void *mmalloc (size_t b)
{
  void *r;
  printf ("mmalloc(%ul) =", (unsigned long) b);
  r = malloc (b);
  printf (" %p\n", r);
  return r;
}
