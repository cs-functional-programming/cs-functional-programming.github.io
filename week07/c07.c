#include <stdio.h>

int fib(int x)
{
  if (x == 0) {
    return 0;
  } else if (x == 1) {
    return 1;
  } else {
    return fib(x - 2) + fib(x - 1);
  }
}

int f(int x)
{
  return 2;
}

int ite(bool b, int e1, int e2)
{
  if (b) {
    return e1;
  } else {
    return e2;
  }
}

int main()
{
  // printf("f(fib(100)) == %d\n", f(fib(100)));
  int z = 3;
  printf("f(fib(100)) == %d\n", ite(z > 20, fib(100), 13));
  printf("f(fib(100)) == %d\n", z > 20 ? fib(100) : 13);
  return 0;
}
