enum X {
 x1, x2, x3
 };

enum Y {
 y1, y2, y3
 };

main () {
  enum X i;
  enum Y k;
  float d;
  double dd;
  int tmp;
  int *tmp2;
  tmp = (k && i) && tmp2 && (i >> k) || (k << i) || dd && d;
}
