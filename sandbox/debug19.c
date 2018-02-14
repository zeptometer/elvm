#include <stdio.h>

#define test(a, b) do {\
    putchar('0' + (a));				\
    putchar('=');				\
    putchar('>');				\
    putchar('0' + (b));				\
    putchar('\n');				\
  } while (0)

int main() {
  test(1 == 0, 0);
  test(0 < 10, 1);
  test(-1 == -1, 1);
  test(-296 < -1, 1);
  test(-296 >= -1, 0);
  test(296 >= -1, 1);
  test(296 >= -1, 1);
  test(-296 >= 11, 0);
}
