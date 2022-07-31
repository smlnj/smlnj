extern printf ();

main ()
{
  int i = 10;

  while (i--) {
    struct {int x; int y;} point;
    printf ("i=%d\n",i);
  }

  return 0;
}


