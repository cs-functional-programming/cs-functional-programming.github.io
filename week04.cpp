#include <iostream>

// let us add division here
// there is just one thing to do: add a new deriving class

// let us add simplification here

using namespace std;

class AExp
{
public:
  virtual int evaluate() = 0;
  virtual AExp *simplify() = 0;
  virtual bool isZero() = 0; // is this the proper way
                             // in C++?
  // no, because you are leaking implementation details
};

class ConstExp : public AExp
{
public:
  int c;
  ConstExp(int c) : c(c) {}
  virtual bool isZero() override
  {
    return c == 0;
  }
  virtual int evaluate() override
  {
    return c;
  }
  virtual AExp* simplify() override
  {
    return this;
  }
};

class PlusExp : public AExp
{
public:
  AExp *e1, *e2;
  PlusExp(AExp *e1, AExp *e2) : e1(e1), e2(e2) {}
  virtual bool isZero() override
  {
    return false;
  }
  virtual int evaluate() override
  {
    return e1->evaluate() + e2->evaluate();
  }
  virtual AExp *simplify() override
  {
    AExp *e1p = e1->simplify();
    AExp *e2p = e2->simplify();
    if (e1p->isZero()) {
      return e2p;
    } else if (e2p->isZero()) {
      return e1p;
    } else {
      return new PlusExp(e1p, e2p);
    }
  }
};

class MinusExp : public AExp
{
public:
  AExp *e1, *e2;
  MinusExp(AExp *e1, AExp *e2) : e1(e1), e2(e2) {}
  virtual bool isZero() override
  {
    return false;
  }
  virtual int evaluate() override
  {
    return e1->evaluate() - e2->evaluate();
  }
  virtual AExp *simplify() override
  {
    AExp *e1p = e1->simplify();
    AExp *e2p = e2->simplify();
    return new MinusExp(e1p, e2p);
  }
};

class TimesExp : public AExp
{
public:
  AExp *e1, *e2;
  virtual bool isZero() override
  {
    return false;
  }
  TimesExp(AExp *e1, AExp *e2) : e1(e1), e2(e2) {}
  virtual int evaluate() override
  {
    return e1->evaluate() * e2->evaluate();
  }
  virtual AExp *simplify() override
  {
    AExp *e1p = e1->simplify();
    AExp *e2p = e2->simplify();
    return new TimesExp(e1p, e2p);
  }
};

class DivisionExp : public AExp
{
public:
  AExp *e1, *e2;
  DivisionExp(AExp *e1, AExp *e2) : e1(e1), e2(e2) {}
  virtual bool isZero() override
  {
    return false;
  }
  virtual int evaluate() override
  {
    return e1->evaluate() / e2->evaluate();
  }
  virtual AExp *simplify() override
  {
    AExp *e1p = e1->simplify();
    AExp *e2p = e2->simplify();
    return new DivisionExp(e1p, e2p);
  }
};

class UnaryMinusExp : public AExp
{
public:
  AExp *e;
  UnaryMinusExp(AExp *e) : e(e) {}
  virtual bool isZero() override
  {
    return false;
  }
  virtual int evaluate() override
  {
    return -e->evaluate();
  }
  virtual AExp *simplify() override
  {
    AExp *ep = e->simplify();
    return new UnaryMinusExp(ep);
  }
};

class VariableExp : public AExp
{
public:
  string variableName;
  VariableExp(string vname) : variableName(vname) {}
  virtual bool isZero() override
  {
    return false;
  }
  virtual int evaluate() override
  {
    return 0;
  }
  virtual AExp *simplify() override
  {
    return this;
  }
};

int main()
{
  // AExp *e =
  //   new TimesExp(
  //     new PlusExp(
  //       new ConstExp(7),
  //       new VariableExp("x")),
  //     new PlusExp(
  //       new ConstExp(3),
  //       new VariableExp("y")));
  AExp *e =
    new TimesExp(
      new DivisionExp(
        new ConstExp(7),
        new ConstExp(2)),
      new PlusExp(
        new ConstExp(3),
        new VariableExp("y")));
  cout << e->evaluate() << endl;
                  //(Times (Plus (Const 7) (Variable "x")) (Plus (Const 3) (Variable "y"))) myDictionary
}


