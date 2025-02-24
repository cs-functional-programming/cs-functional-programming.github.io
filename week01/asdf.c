#include <stdio.h>
#include <stdlib.h>

int gg = 0;

int g(int x)
{
  printf("Apel 1: asdf\n");
  exit(0);
  gg++;
  if (gg == 2) {
    exit(0);
  }
  return x + gg;
}

int main()
{
  printf("Apel 1: %d\n", g(0));
  printf("Apel 2: %d\n", g(0));
}
