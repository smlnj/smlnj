struct X {
  int i;
  int j;
} k;

struct X f() {
  return(k);
}

main () {
  (f()).i = 1;
}
