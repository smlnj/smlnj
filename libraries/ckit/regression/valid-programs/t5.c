int x;

long y;

extern unsigned long foo (int x);
extern int main ();

unsigned long foo (int x) {
  unsigned long i;

  i = (unsigned long) x;

  return (x);
}

main ()
{
  int i;

    switch (i) {
      case 2: foo (i); break;
      case 3: foo (i); break;
      case 5: foo (i); break;
      case 7: foo (i); break;
      default: foo (i*2);
    }
}

