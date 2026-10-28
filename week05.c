#include <stdio.h>

int X;

int f(int x, int y)
{
  return 2 * x + y;
}

int main()
{
  int (*myfunc)(int, int) = f;
  // WHAT CAN YOU DO with "f"?
  // (1) we can call "f"
  // (2) we can take its address
  // (2') you can copy the address and call the function via the copy
  // (2'') pass the pointer to other functions
  int result = myfunc(3, 4);

  // WHAT CAN YOU DO WITH "X"?
  // (1) we can get the value of X and process it
  int interesting = X + 7;
  // (2) we can get the address of X
  int *address = &X;
  // (3) we can create values on the spot and use them to update X
  X = 13;

  // In C, functions are not first-class citizens.
}
