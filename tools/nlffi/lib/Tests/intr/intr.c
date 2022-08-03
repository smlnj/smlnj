# include <stdio.h>

static void g (int i)
{
  if (i == 0)
    ;
  else {
    g (i - 1);
    g (i - 1);
  }
}

void f (int i)
{
  printf ("Starting f(%d)\n", i);
  g (i);
  printf ("Done with f(%d)\n", i);
}
