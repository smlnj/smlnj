struct foo { int a[10];
             int p;
           };

int b[100];

int f (int a[100]) {
  return a[0];
}

main () {
  struct foo j;
  return j.p;
}

