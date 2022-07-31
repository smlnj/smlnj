static int g() {
  return 0;
}

static int f(int h()) {
/*  x = 1; */
  return((************h)());
}

int main () {
  return(f(g));
}


