/*****
NUMBER: 4
SUBMITTER: Alexey Loginov <alexey@cs.wisc.edu>
DATE: 2/10/00
TEST: 
STATUS: 
DESCRIPTION:
  old style C function parameter declarations have different
  semantics from new style (with respec to promotions) and
  should be preserved.

   void foo(int);
   void foo(c)
   char c; { }

	--> compiles, but

   void foo(int);
   void foo(char c) { }

	--> which is output by ckit, does not
***/

void foo(int);
void foo(c)
 char c;
{
 return;
}

void main() { 
 return;
}
