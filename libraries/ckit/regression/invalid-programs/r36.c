struct X {
 int f;
 const char c;
} x, *xx;

main () {
 const char c = 'c';
 x.c = c;
 xx = &x;
 xx -> c = c;
}
