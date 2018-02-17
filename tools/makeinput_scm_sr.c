#include <stdio.h>

void
to_binary(char c, char* buff) {
  for (int i = 0; i < 24; i++) {
    buff[i * 2] = (c & 1)?'1':'0';
    buff[i * 2 + 1] = ' ';
    c >>= 1;
  }
  buff[47] = '\0';
}

int
main() {
  printf("(define-syntax input!\n");
  printf("  (syntax-rules (quote)\n");
  printf("    ((_ s) (ck s '(\n");

  char c;
  while ((c = getchar()) != EOF) {
    char buff[100] = "nekodesuyo";
    if (c == 0) {
      printf("                   0\n");
    } else {
      to_binary(c, buff);
      printf("                   (%s)\n", buff);
    }
  }

  printf("                  )))))\n");
}
