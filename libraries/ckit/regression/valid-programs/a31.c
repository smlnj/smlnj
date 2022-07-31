void *f(int x) {
 return((void *) (((int) f) + x));
}

main () {
 f(3);
}
