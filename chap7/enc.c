#include <stdio.h>
#include <string.h>
#include <err.h>

#define PRIME (17)
#define SHIFT (11)

/**
 * @plain: input text
 * @shift: an integer
 * @prime: a prime number
 */
char* ceaser(const char *plain, int shift, int prime) {
  const int len = strlen(plain);
  char *cipher = (char *)malloc(len);
  
  for (int i = 0 ; i < len ; i ++) {
    *(cipher + i) = (*(plain + i) - 'a' + shift) % prime + 'a';
  }
  *(cipher + len) = '\0';

  return cipher;
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    err(1, "usage: %s [text]\n", argv[0]);
  }

  const char *encrypted = ceaser(argv[1], SHIFT, PRIME);
  const char *decrypted = ceaser(encrypted, -SHIFT, PRIME);

  printf("'%s' =enc->> '%s' =dec->> '%s'\n",
	 argv[1], encrypted, decrypted);
  
  return 0;
}
