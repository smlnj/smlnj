
static char mimbar (char c_p1437)
{
  
  return c_p1437;
}
void main ()
{
  static char c_p1439;
  static char (*f) (char);
  char call_p1441;
  f = mimbar;
  call_p1441 = f (c_p1439);
  c_p1439 = call_p1441;
}
