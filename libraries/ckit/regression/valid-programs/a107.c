int t (int const *,const int);

int main () {
  int const volatile j[10];
  int const i = 4;
  int const * volatile const *p;
  return (i-i);
}

