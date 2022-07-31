struct foo { int a[10];
             int p : 4;
           };

int main () {
  struct foo j;
  return j.p;
}

