struct foo {int x; float y;};
struct foo ast1 = {1,1.0};
int z[3];


int main ();

/* int (*f)() = main; */

int main (){
  struct foo y = ast1;
  return 0;
}

