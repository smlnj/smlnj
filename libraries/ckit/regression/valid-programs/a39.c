/* a39.c */
/* Fisher bug, 5/20/99 */

struct {
  int count[3];
} *p;

int main(){
  int i = 0;
  p->count[i]++; /* generates an error. */
  p->count[i] = p->count[i]+1; /* OK */
  return 0;
}
