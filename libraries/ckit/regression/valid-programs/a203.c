void printf();

struct foo {char x; int y;} a, b;

main (){

  char x;
  int y;

  x = 255;

  y = (x = x + 1);

  printf ("y=%d\n",y);

  x = 255;
  x = x + 1;
  y = x;

  printf ("y=%d\n",y);


  return 0;
}
