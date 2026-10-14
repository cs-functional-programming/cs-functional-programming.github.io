#include <stdio.h>

typedef enum Dow { Mon, Tue, Wed } Dow;

int main()
{
  Dow day = 8; // enums in C are not type-safe
  printf("%d\n", day);
}
