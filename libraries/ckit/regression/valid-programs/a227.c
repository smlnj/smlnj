/* "bug 8": promotion of arrays inside ?: */

void printf();

int main() {
   int *ip;
   int *jp;
   int ia[3];
   jp = (1 ? ia : ip);
 }
