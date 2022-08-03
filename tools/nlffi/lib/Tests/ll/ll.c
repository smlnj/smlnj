# include <stdio.h>
# include "ll.h"

struct s s = { 0, 0 };

void ps (void)
{
  printf ("%llx %lld\n", s.u, s.s);
}

void pll (int a, long long i, int b)
{
  printf ("%d %lld %d\n", a, i, b);
}

long long a (int i1, long long l1, int i2, long long l2, int i3, long long l3, int i4, long long l4)
{
  return i1 + l1 + i2 + l2 + i3 + l3 + i4 + l4;
}
