
int main ()
{
  int *p_p275;
  int *post_p276;
  post_p276 = p_p275;
  p_p275 = ((int *) (((int) p_p275)+4));
  post_p276;
  p_p275 = (p_p275+3);
}
