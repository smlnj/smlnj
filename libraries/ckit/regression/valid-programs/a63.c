void f(int x[4]);

main () {
 int y[4];
 f(y);
} 

void f(int x[4]) {
 int *y;
 x = y;
 x[3] = 1;
}
