struct X {
 int x1;
 };

struct Y {
 int x2;
 };

main () {
  struct X x;
  struct Y y;
  int tmp;

  tmp = (x && y);
}
