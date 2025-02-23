#include <iostream>
#include <cmath>

class Exp
{
public:
  virtual int eval() const = 0;
  virtual Exp *simpl() = 0;
};

class ExpConst : public Exp
{
public:
  int x;

  ExpConst(int x) : x(x) {}
  
  virtual int eval() const
  {
    return x;
  }

  virtual Exp *simpl()
  {
    return this;
  }
};

class ExpAdd : public Exp
{
public:
  Exp *e1;
  Exp *e2;

  ExpAdd(Exp *e1, Exp *e2) :
    e1(e1), e2(e2) {}
  
  virtual int eval() const
  {
    return e1->eval() + e2->eval();
  }
  
  virtual Exp *simpl()
  {
    return new ExpAdd(e1->simpl(), e2->simpl());
  }
};

class ExpPow : public Exp
{
public:
  Exp *e1;
  Exp *e2;

  ExpPow(Exp *e1, Exp *e2) :
    e1(e1), e2(e2) {}
  
  virtual int eval() const
  {
    return pow(e1->eval(), e2->eval());
  }

  virtual Exp *simpl() 
  {
    return new ExpPow(e1->simpl(), e2->simpl());
  }
};

int main()
{
  Exp *e = new ExpAdd(new ExpConst(7), new ExpConst(8));
  return -1;
}
