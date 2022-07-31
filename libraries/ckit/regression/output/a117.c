
extern int getopt ();
int main (int argc_p116,int argv_p117)
{
  int i_p118;
  int call_p122;
  goto whileCont_p120;
  
whileTop_p119: 
  ;
  goto whileCont_p120;
  
whileCont_p120: 
  ;
  call_p122 = getopt (argc_p116,argv_p117,"c:a:f:F:");
  i_p118 = call_p122;
  if (i_p118!=(-1)) 
    goto whileTop_p119;
  return 0;
}
