
int main ()
{
  int i_p615;
  int a_p616;
  int post_p622;
  int post_p626;
  int post_p630;
  i_p615 = 1;
  a_p616 = 1;
  goto forStart_p619;
  
forTop_p618: 
  ;
  a_p616 = (a_p616*i_p615);
  post_p622 = i_p615;
  i_p615 = (i_p615+1);
  
forStart_p619: 
  ;
  if (i_p615<=6) 
    goto forTop_p618;
  printf ("fact 6 = %d\n",a_p616);
  i_p615 = 1;
  a_p616 = 1;
  goto whileCont_p624;
  
whileTop_p623: 
  ;
  post_p626 = i_p615;
  i_p615 = (i_p615+1);
  a_p616 = (a_p616*post_p626);
  
whileCont_p624: 
  ;
  if (i_p615<=7) 
    goto whileTop_p623;
  printf ("fact 7 = %d\n",a_p616);
  i_p615 = 1;
  a_p616 = 1;
  
doTop_p627: 
  ;
  post_p630 = i_p615;
  i_p615 = (i_p615+1);
  a_p616 = (a_p616*post_p630);
  if (i_p615<=8) 
    goto doTop_p627;
  printf ("fact 8 = %d\n",a_p616);
}
