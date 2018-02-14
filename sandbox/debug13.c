#include <stdio.h>

#define test(a, b) do {\
    putchar('0' + (a));				\
    putchar('=');				\
    putchar('>');				\
    putchar('0' + (b));				\
    putchar('\n');				\
  } while (0)

int main() {
  test(0 < 10, 1);
  test(28 == 28, 1);
  test(28 == 27, 0);
  test(28 <= 28, 1);
  test(28 <= 29, 1);
  test(0 <= 29, 1);
  test(28 <= 9999, 1);
  test(28 < 28, 0);
  test(28 < 29, 1);
  test(0 < 29, 1);
  test(28 < 9999, 1);
  test(28 <= 27, 0);
}
