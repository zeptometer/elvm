int putchar(int);

int main() {
  for(int i = 0; i < 10; i++)
    putchar('0' + i);
  putchar('\n');
  return 1;
}
