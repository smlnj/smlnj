main () {
  int a[10];
  int *(p[10]);

  *p = a;
  a = 4;
  (*p) = 4;
}
