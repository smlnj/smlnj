typedef void* Word_t;
enum loc_kind {GPR=0, FPR, STK, FSTK };
struct located_arg_s {
  enum loc_kind k;
  int width;
  int narrowing;
  int loc;
  int offset;
  union {
    Word_t* p;
    long l;
    int i;
    char* s;
    double d;
  } arg;
};
