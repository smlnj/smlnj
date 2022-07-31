
struct foo_t27 {
  char x;
  int y;
};
struct foo_t27 a;
struct foo_t27 b;
int main ()
{
  int *p_p244;
  p_p244 = ((int *) (&a));
  *p_p244 = (-1);
  b = a;
  p_p244 = ((int *) (&b));
  printf ("*p=%x\n",*p_p244);
  return 0;
}
