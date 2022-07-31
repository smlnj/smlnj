int f(int (*)[4]);

int f(int (*)[]);

main () {
  int (*a)[5];
  f(a);
}
