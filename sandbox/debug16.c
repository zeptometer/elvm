int putchar(int);

#define test(a, b) do {\
    putchar('0' + (a));				\
    putchar('=');				\
    putchar('>');				\
    putchar('0' + (b));				\
    putchar('\n');				\
  } while (0)

int main() {
  test(1231 != 1231, 0);
  test(1231 != 1232, 1);
  test(1230 != 1231, 1);
  test(1231 != 8000, 1);
  test(8000 != 1231, 1);
  return 1;
}
