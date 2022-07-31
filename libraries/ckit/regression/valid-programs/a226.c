/* "bug 10": preincrements get turned into postincrements */

void printf();

int main() {
  int i = 10;
  int i1 = ++i;
  int i2 = --i;

  printf("%d, %d, %d\n", i, i1, i2);
  return 1;
}
