/* "bug 1": enum constants */
void printf();

main() {

  enum {e1,e2,e3} e;
  printf ("e1=%d,e2=%d,e3=%d\n",e1,e2,e3);
  return 1;

}
