
struct foo_t28 {
  char x;
  int y;
};
struct foo_t28 a;
struct foo_t28 b;
int main ()
{
  char x_p252;
  int y_p253;
  x_p252 = ((char) 255);
  x_p252 = ((char) (x_p252+1));
  y_p253 = ((int) x_p252);
  printf ("y=%d\n",y_p253);
  x_p252 = ((char) 255);
  x_p252 = ((char) (x_p252+1));
  y_p253 = ((int) x_p252);
  printf ("y=%d\n",y_p253);
  return 0;
}
