struct X {
  int x1;
  int x2;
  int x3;
};

enum Y {
  x1, x2, x3
};

main() {
  struct X y, z;
  void *p;
  float x2;
  char *i;
  
  p = &x2;
  p = &y;
  i = p;
}
