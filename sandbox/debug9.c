#include <stdio.h>

void printnum(int n) {
  if (n >= 10) {
    printnum(n / 10);
  }
  putchar('0' + (n % 10));
}

int main() {
  printnum(200);
  putchar('0' + 3);
  putchar('\n');
  return 1;
}
