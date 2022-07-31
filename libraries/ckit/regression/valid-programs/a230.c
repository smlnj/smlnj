/* 
NUMBER: 12
SUBMITTER: Kathleen Fisher <kfisher@research.att.com>
DATE: 3/15/00

We've run into a problem with ckit when we turn the flag
convert_function_args_to_pointers to false in the config.sml file.
The following program:

********************************************************
typedef int *windowTy[1];

int f (windowTy w)
{
 return 1;
}

void main(){
  windowTy w;
  f (w);
}
********************************************************

compiles just fine using cc, but it generates the following
error if we compile it with ckit:

"array-param.hc":11.3-8: error: Bad function call: arg 1 has type windowTy
but fn parameter has type windowTy
**/

typedef int *windowTy[1];

int f (windowTy w)
{
 return 1;
}

void main(){
  windowTy w;
  f (w);
}
