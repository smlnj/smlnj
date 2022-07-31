void p();
void q();
void printf();

enum nevin {X=3, Y=10};

int i;

main () {
  p();
  printf("%d\n", i);
  q();
  printf("%d\n", i);
}

void p() {
  enum dino { L=1, S=2};

  i = L;
}

void q() {
  enum dino { L=3, S=4};
  i = L;
}
