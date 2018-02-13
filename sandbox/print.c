int putchar(int);

int main() {
  char* buf = "Hello, world!\n";
  do {
    putchar(*buf);
  } while (*(++buf));
  return 0;
}
