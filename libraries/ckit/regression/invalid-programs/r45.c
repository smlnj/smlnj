int f(int (*)[]);

int f(int (*)[4]);

main () {
  int (*a)[5];
  f(a);
}
