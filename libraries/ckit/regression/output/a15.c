
int main ()
{
  int j_p161;
  int *i_p162;
  int *post_p163;
  j_p161 = 1;
  i_p162 = (&j_p161);
  post_p163 = i_p162;
  i_p162 = ((int *) (((int) i_p162)+4));
  *post_p163 = 3;
}
