#include "located-arg.h"
#include <stdio.h>
#include <stdlib.h>

int main () {

  char* s = "test\n";

  struct located_arg_s larg = {
    STK,
    32,
    32,
    0,
    0,
    s
  };

  VarargInterp(printf, &larg, &larg+1);
  return 0;
}
