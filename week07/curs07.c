#include <stdio.h>

int ite(bool b, int x, int y)
{
  if (b) {
    return x;
  } else {
    return y;
  }
}

int fib(int n)
{
  if (n <= 1) {
    return n;
  } else {
    return fib(n - 2) + fib(n - 1);
  }
}

int a = compute();

int compute()
{
  int x = fib(100);
  return 2 * 2;
}

int main()
{
  printf("Rezultat: %d\n", compute());
  return 0;
}
