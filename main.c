#include <iostream>
#include <string>

using namespace std;

int main()
{
  string name;
  cout << "What is your name?";
  while (1)
  {
    cin >> name;
    cout << "What is your name?";
  }
}
