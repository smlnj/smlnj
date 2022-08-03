union u { int i; } u = { 42 }, u2 = { 0 };

union u uf (union u u)
{
  union u ru;
  printf ("uf: %d\n", u.i);
  ru.i = u.i + 10;
  return ru;
}

int f1 (void)
{
  return 1;
}

float f2 (void)
{
  return 2.0;
}

double f3 (void)
{
  return 3.0;
}

int addii (int x, int y)
{
  return x + y;
}

float addif (int x, float y)
{
  return x + y;
}

double addid (int x, double y)
{
  return x + y;
}

double adddi (double x, int y)
{
  return x + y;
}

double projid2 (int x, double y)
{
  return y;
}

double projsid3 (char *s, int x, double y)
{
  return y;
}
