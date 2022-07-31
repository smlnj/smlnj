/* Bug reported by Olivier; 
   used to give:
     error: Type Error: operand of unary op ! must be a number.
   Problem: 
*/

main() {
  void* p;

  !p;
}

