int *f() {
 int i = 1;
 return(&i);
}

main () {
  void *j;
  j = f();
}
