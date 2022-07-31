extern printf ();

extern int i;

int foo () {

  return i;
}

int i = 10;

main () {
  printf ("foo = %d\n",foo ());
  printf ("i = %d\n",i);
  return 0;
}
