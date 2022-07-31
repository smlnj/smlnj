
int main ()
{
  int x_p278;
  int pref_p281;
  int y_p279;
  int pref_p282;
  int volatile j_p280;
  x_p278 = 1;
  x_p278 = (x_p278+1);
  pref_p281 = x_p278;
  y_p279 = pref_p281;
  x_p278 = (x_p278+1);
  pref_p282 = x_p278;
  j_p280 = pref_p282;
  return 0;
}
