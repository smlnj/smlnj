extern int narn (int,int,int);

int narn (int x, int y, int z)
{
  return 1;
}

void main ()
{ 
  register int j;

  static int k;

  {int k;
   k = narn (k,j,j);
   j = k;
  }

  return;
}




