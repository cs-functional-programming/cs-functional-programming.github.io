#include <iostream>
#include <string>

// -- Modificare (1)
// -- Vreau sa adaug un nou caz, pentru impartire
// -- adaug o noua clasa (SIMPLU)

// -- Modificare (2)
// -- Vreau sa adaug o noua functie, de simplificare (GREU)


using namespace std;

class Expr
{
public:
  virtual int eval() = 0;
  virtual Expr *simpl() = 0;
  virtual bool isZero() = 0;
};

class ConstExpr : public Expr
{
public:
  int c;
  ConstExpr(int c) : c(c) {}
  virtual int eval()
  {
    return c;
  }
  virtual Expr *simpl()
  {
    return new ConstExpr(c);
  }
  virtual bool isZero()
  {
    return c == 1;
  }
};
  
class VarExpr : public Expr
{
public:
  string s;
  VarExpr(string s) : s(s) {}
  virtual int eval()
  {
    return 1;
  }
  virtual Expr *simpl()
  {
    return new VarExpr(s);
  }
  virtual bool isZero()
  {
    return false;
  }
};

class PlusExpr : public Expr
{
public:
  Expr *l;
  Expr *r;
  PlusExpr(Expr *l, Expr *r) : l(l), r(r) {}
  virtual int eval()
  {
    return l->eval() + r->eval();
  }
  virtual Expr *simpl()
  {
    if (l->isZero()) {
      return r->simpl();
    } else if (r->isZero()) {
      return l->simpl();
    } else {
      return new PlusExpr(l, r);
    }
  }
  virtual bool isZero()
  {
    return false;
  }
};
  
class MultExpr : public Expr
{
public:
  Expr *l;
  Expr *r;
  MultExpr(Expr *l, Expr *r) : l(l), r(r) {}
  virtual int eval()
  {
    return l->eval() * r->eval();
  }
  virtual bool isZero()
  {
    return false;
  }
  virtual Expr *simpl()
  {
    return new MultExpr(l, r);
  }
};
  
class MinusExpr : public Expr
{
public:
  Expr *l;
  Expr *r;
  MinusExpr(Expr *l, Expr *r) : l(l), r(r) {}
  virtual int eval()
  {
    return l->eval() - r->eval();
  }
  virtual bool isZero()
  {
    return false;
  }
  virtual Expr *simpl()
  {
    return this;
  }
};

class ImpartireExpr : public Expr
{
public:
  Expr *l;
  Expr *r;
  ImpartireExpr(Expr *l, Expr *r) : l(l), r(r) {}
  virtual int eval()
  {
    return l->eval() / r->eval();
  }
  virtual bool isZero()
  {
    return false;
  }
  virtual Expr *simpl()
  {
    return this;
  }
};

int main()
{
  Expr *e1 = new PlusExpr(
               new PlusExpr(
                 new MultExpr(
                   new VarExpr("x"),
                   new ConstExpr(2)),
                 new MultExpr(
                   new MinusExpr(
                     new MultExpr(
                       new VarExpr("y"),
                       new ConstExpr(10)),
                     new VarExpr("z")),
                   new VarExpr("z")
                 )
               ),
               new ConstExpr(27));
  cout << e1->eval() << endl;
  return 0;
}
