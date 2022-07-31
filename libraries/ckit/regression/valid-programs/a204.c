void printf();

struct foo {int a[3]; int b;} w[] = {{1},2};

struct foo z[] = {{2,3,4},{5},{1,42,44,13},1,2,2,2,{3},4};

main (){

  int i, j; 

  for (i=0; i<(sizeof(z)/16); i++)
    {
      printf ("a=" );
      for (j=0;j<3;j++)
	{
	  printf ("%10d ",z[i].a[j]);
	}
      printf ("b=%d\n",z[i].b);
    }

}
