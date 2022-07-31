struct X { 
  int x;
  int y;
}

main(){
  register struct X p;
  p.x = 1;
  &p;
}
