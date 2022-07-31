void printf();

struct foo {char x; int y;} a, b;

main (){

  int *p;

  p = (int *) &a;

  *p = -1;

  b = a;
  /*
  b.x = a.x;
  b.y = a.y;
*/

  p = (int *) &b;

  printf ("*p=%x\n",*p);

  return 0;
}
