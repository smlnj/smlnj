
main ()
{
  int i;
  int x1;
  int *x2;
  int *x3[3];
  int **x4;
  int (*x5)();



  x1 = (int) i;
  x2 = (int *) i;
  /*x3 = (int *[3]) x3;*/
  x4 = (int **) x4;
  x5 = x5;

  /*  x = ((*[ ])(void)) i; */
}

