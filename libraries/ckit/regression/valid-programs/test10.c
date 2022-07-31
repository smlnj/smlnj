void printf();

int i = 2;
signed int i;

struct {int a; int b;} x;
struct {int a; int b;} y;
struct S {int a; int b; } u;
struct S v;

main() {
  {
    int i=35;
    struct {int a; int b;} x;
    struct {int a; int b;} y;
    struct S {int a; int b; } u;
    struct S v;
    v.a = 1; 
    v.b = 34;
    u = v;
    printf("%d\n", i);
  }
}
