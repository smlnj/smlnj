enum E1 {
 x, y, z
};

enum E2 {
 a, b
};

int f(enum E1);

int f(enum E2);

main () {
  f(a);
}
