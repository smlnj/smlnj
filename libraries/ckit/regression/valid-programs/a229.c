void printf();

struct X1 {char x; char z;} X1x[10];
struct X9 {int :0; char x; char y;} X9x[10];
struct X2 {char x; char y; char z;} X2x[10];

struct X3 {int :6; char x; char y;} X3x[10];
struct X4 {char x; int :6; char y;} X4x[10];

struct X5 {int q:6; char x; char y;} X5x[10];
struct X6 {char x; int q:6; char y;} X6x[10];

struct X10 {char x; int :0; char y;} X10x[10];

struct X7 {int x; char y; int z;} X7x[10];
struct X8 {int x; char y; int z; char q;} X8x[10];

main () {
  printf("%d, %d\n", sizeof(struct X1), sizeof(X1x));
  printf("%d, %d\n", sizeof(struct X9), sizeof(X9x));
  printf("%d, %d\n", sizeof(struct X2), sizeof(X2x));
  printf("%d, %d\n", sizeof(struct X3), sizeof(X3x));
  printf("%d, %d\n", sizeof(struct X4), sizeof(X4x));
  printf("%d, %d\n", sizeof(struct X5), sizeof(X5x));
  printf("%d, %d\n", sizeof(struct X6), sizeof(X6x));
  printf("%d, %d\n", sizeof(struct X10), sizeof(X10x));
  printf("%d, %d\n", sizeof(struct X7), sizeof(X7x));
  printf("%d, %d\n", sizeof(struct X8), sizeof(X8x));
}

