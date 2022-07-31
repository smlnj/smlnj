int f(int (*)[]);

int f(int (*x)[4]) {
 return(0);
}

main () {
  int (*a)[5];
  f(a);
}


