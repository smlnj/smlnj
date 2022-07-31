
void main ()
{
  int i_p1443;
  int post_p1449;
  i_p1443 = 0;
  goto forStart_p1446;
  
forTop_p1445: 
  ;
  switch (i_p1443)
    {
      
      
    case 0: 
      goto forCont_p1447;
      
    default: 
      goto switchBrk_p1450;
    }
  
switchBrk_p1450: 
  ;
  if (i_p1443==4) 
    goto forBrk_p1448;
  printf ("i = %d\n",i_p1443);
  
forCont_p1447: 
  ;
  post_p1449 = i_p1443;
  i_p1443 = (i_p1443+1);
  
forStart_p1446: 
  ;
  if (i_p1443<10) 
    goto forTop_p1445;
  
forBrk_p1448: 
  ;
}
