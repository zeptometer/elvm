#include <stdio.h>

void test(int val, int expected) {
  printf("%c -> %c\n", (expected?'T':'F'), (val?'T':'F'));
}

int main() {
  test(1231 != 1231, 0);
  test(1231 != 1232, 1);
  test(1230 != 1231, 1);
  test(1231 != 8000, 1);
  test(8000 != 1231, 1);
  return 1;
}
