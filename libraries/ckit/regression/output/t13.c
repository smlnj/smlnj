
void main ()
{
  int i_p1452;
  int post_p1457;
  i_p1452 = 0;
  
f1_p1456: 
  ;
  post_p1457 = i_p1452;
  i_p1452 = (i_p1452+1);
  if (post_p1457<4) 
    goto f2_p1453;
  else
    goto f3_p1454;
  
f2_p1453: 
  ;
  printf ("i = %d\n",i_p1452);
  goto f1_p1456;
  
f3_p1454: 
  ;
  printf ("i\'= %d\n",i_p1452);
}
