/* "bug 1": enum constants */
void printf();

main() {

  enum {e1,e2=10,e3=-e2,e4=20} e;
  printf ("e1=%d,e2=%d,e3=%d,e4=%d\n",e1,e2,e3,e4);
  return 1;

}
