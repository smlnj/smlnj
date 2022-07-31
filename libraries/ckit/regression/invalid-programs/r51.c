int f(const int x[2]);

int f(int x[3]) {
/*  x = 1; */
  return(x[0]);
}

main () {
  int i[4];
  f(i);
}


