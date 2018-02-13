#include <stdio.h>

int fizzbuzz(int n) {
  if (n <= 1)
    return 1;
  else
    return fizzbuzz(n - 1) + fizzbuzz(n - 2);
}

int main() {
  printf("%d\n", fizzbuzz(10));
  return 1;
}
