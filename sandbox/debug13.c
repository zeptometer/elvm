#include <stdio.h>

void test(int val, int expected) {
  printf("%c -> %c\n", (expected?'T':'F'), (val?'T':'F'));
}

int main() {
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
