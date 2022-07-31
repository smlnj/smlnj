/* test for "bug 5": externs and union/struct initializers */

struct foo {int x; float y;};

main (){
  struct foo ast1 = {1,1.0};
  struct foo y = ast1;
  extern int bar;
  return 0;
}
