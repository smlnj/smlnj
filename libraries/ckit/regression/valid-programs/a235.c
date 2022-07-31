int fun();

int (*ptr)() = fun;

int fun() {
  return 0;
}

void main () {
  return;
}
