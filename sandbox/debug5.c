int putchar(int);

int main() {
  int i = 0;
  for(; i < 10; i++)
    putchar('0' + i);
  putchar('\n');
  return 1;
}
