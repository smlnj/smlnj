int f(int x) {
  return(x);
}

main () {
  int (*g)(float x) = f;

  f(1.1);
}


