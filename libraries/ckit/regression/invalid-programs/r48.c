int (*(f(int)))[];
int (*(f(int)))[4];

int (*(f(int x)))[3] {
 int (*a)[3];
 return(a);
}

main () {
  int (*a)[5];
  f(2);
}


