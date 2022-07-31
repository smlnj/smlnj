
int f (int (*)[]);
int main ()
{
  int (*a_p393)[5];
  f ((int (*)[]) a_p393);
}
int f (int (*a_p395)[4])
{
  
  return a_p395[0][3];
}
