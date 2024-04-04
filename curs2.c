#include <stdio.h>
#include <stdlib.h>

int f(int x, int y)
{
  static int z = 0;
  if (z == 1) {
    printf("Apel 2: asdf\n");
    exit(-1);
  }
  return x + y + (z++);
}

int main()
{
  printf("Apel 1: %d\n", f(3, 4));
  printf("Apel 2: %d\n", f(3, 4));
  int s = 0;
  for (int i = 0; i < 10; ++i) {
    s += i;
  }
  return -1;
}
