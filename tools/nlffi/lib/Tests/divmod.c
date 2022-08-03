struct dmres {
  int d;
  int m;
};

struct dmres divmod (int x, int y)
{
  struct dmres r;
  r.d = x / y;
  r.m = x % y;
  return r;
}
