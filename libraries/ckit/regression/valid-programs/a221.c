struct foo {int x; float y;};

main (){
  static struct foo ast1 = {1,1.0};
  struct foo y = ast1;
  return 0;
}
