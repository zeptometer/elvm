int putchar(int);

int main() {
  goto hoge;
  putchar('A');
 hoge:
  putchar('B');

  return 1;
}
