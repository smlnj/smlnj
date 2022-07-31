int f(int (*)[]);

main () {

  int b [10][5];
  int (*a)[5];

  a = b;

  f(a);
}

int f(int (*a)[4]) {
  return (a[0][3]);
}

