#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
  int t = time(0);
  printf("%d\n", t);
  srand(t);
  int n1 = rand() % 10;
  int n2 = rand() % 10;
  printf("n1: %d\n", n1);
  printf("n2: %d\n", n2);
  return 0;
}
