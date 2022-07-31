void printf();

char d[] = "asdfasdf";
const char dd[] = "asdfasdf";

main() {
  int i;
  char c[] = "asdfasdf";
  const char cc[] = "asdfasdf";

  for(i=0; i<8; i++) {
    printf("%c %c\n", c[i], cc[i]);
  }
  
  return 1;
}
