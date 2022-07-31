#include "located-arg.h"
#include <stdio.h>
#include <stdlib.h>

void VarargInterp (void* cFun, struct located_arg_s* locdArgs, struct located_arg_s* locdArgsEnd)
{
  printf("sizeof(struct located_args_s)=%d l=%d\n", sizeof(struct located_arg_s), (char*)locdArgsEnd-(char*)locdArgs);
  while(locdArgs < locdArgsEnd) {
    printf("k=%d w=%d n=%d l=%d o=%d ", locdArgs->k, locdArgs->width, locdArgs->narrowing, locdArgs->loc, locdArgs->offset);
    switch(locdArgs->k) {
    case GPR:
    case STK:
      printf("a=%d\n", locdArgs->arg.i);
      break;
    case FPR:
    case FSTK:
      printf("a=%f\n", locdArgs->arg.d);
      break;
    }
    locdArgs++;
  }
  return;
}
