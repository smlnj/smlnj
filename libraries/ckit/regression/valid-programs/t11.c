
static char mimbar (char c) {
  return (c);
}

void main ()
{ 
  static char c;
  static char (*f) (char);

  f = mimbar;

  c = (*f) (c);
}




