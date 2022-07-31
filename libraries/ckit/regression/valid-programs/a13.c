enum X {
 x1, x2, x3
 };

enum Y {
 y1, y2, y3
 };

main () {
  enum X i;
  enum Y k;
  int tmp;
  int *tmp2;
  tmp = k % i;
  tmp = k ^ i;
  tmp = k | i;
  tmp = k & i;
  tmp = k >> i;
  tmp = k << i;
  tmp2 = tmp2 + x1;
  tmp2 = tmp2 + k;
}
