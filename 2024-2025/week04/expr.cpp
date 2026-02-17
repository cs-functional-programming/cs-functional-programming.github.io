#include <iostream>
#include <string>
#include <sstream>

using namespace std;

class Expr
{
public:
  virtual int eval() = 0;
  virtual Expr *derivata() = 0;
  virtual Expr *simpl() = 0;
  virtual bool isConst() = 0;
  virtual int getConst() = 0;
  virtual string toString() = 0;
};

class ConstExpr : public Expr
{
private:
  int c;
public:
  ConstExpr(int c) : c(c) {}
  virtual int eval()
  {
    return c;
  }
  virtual Expr *derivata()
  {
    return new ConstExpr(0);
  }
  virtual Expr *simpl()
  {
    return this;
  }
  virtual bool isConst()
  {
    return true;
  }
  virtual int getConst()
  {
    return c;
  }
  virtual string toString()
  {
    ostringstream oss;
    oss << c;
    return oss.str();
  }
};

class VarExpr : public Expr
{
private:
  string var;
public:
  VarExpr(string var) : var(var) {}
  virtual int eval()
  {
    return 0; // todo: use assignment
  }
  virtual Expr *derivata()
  {
    if (var == "x") {
      return new ConstExpr(1);
    } else {
      return new ConstExpr(0);
    }
  }
  virtual Expr *simpl()
  {
    return this;
  }
  virtual bool isConst()
  {
    return false;
  }
  virtual int getConst()
  {
    return 0;
  }
  virtual string toString()
  {
    ostringstream oss;
    oss << var;
    return oss.str();
  }
};

class SumaExpr : public Expr
{
private:
  Expr *e1;
  Expr *e2;
public:
  SumaExpr(Expr *e1, Expr *e2) : e1(e1), e2(e2) {}
  virtual int eval()
  {
    return e1->eval() + e2->eval();
  }
  virtual Expr *derivata()
  {
    return new SumaExpr(e1->derivata(), e2->derivata());
  }
  virtual Expr *simpl()
  {
    Expr *e1p = e1->simpl();
    Expr *e2p = e2->simpl();
    if (e1p->isConst() && e2p->isConst()) {
      return new ConstExpr(e1p->getConst() +
                           e2p->getConst());
    }
    if (e1p->isConst() && e1p->getConst() == 0) {
      return e2p;
    }
    if (e2p->isConst() && e2p->getConst() == 0) {
      return e1p;
    }
    return new SumaExpr(e1p, e2p);
  }
  virtual bool isConst()
  {
    return false;
  }
  virtual int getConst()
  {
    return 0;
  }
  virtual string toString()
  {
    ostringstream oss;
    oss << "Suma(" << e1->toString() << ", " << e2->toString() << ")";
    return oss.str();
  }
};

class ProdusExpr : public Expr
{
private:
  Expr *e1;
  Expr *e2;
public:
  ProdusExpr(Expr *e1, Expr *e2) : e1(e1), e2(e2) {}
  virtual int eval()
  {
    return e1->eval() * e2->eval();
  }
  virtual Expr *derivata()
  {
    return new SumaExpr(
                        new ProdusExpr(e1->derivata(), e2),
                        new ProdusExpr(e1, e2->derivata()));
  }
  virtual Expr *simpl()
  {
    Expr *e1p = e1->simpl();
    Expr *e2p = e2->simpl();
    if (e1p->isConst() && e2p->isConst()) {
      return new ConstExpr(e1p->getConst() *
                           e2p->getConst());
    }
    if (e1p->isConst() && e1p->getConst() == 0) {
      return new ConstExpr(0);
    }
    if (e2p->isConst() && e2p->getConst() == 0) {
      return new ConstExpr(0);
    }
    if (e1p->isConst() && e1p->getConst() == 1) {
      return e2p;
    }
    if (e2p->isConst() && e2p->getConst() == 1) {
      return e1p;
    }
    return new ProdusExpr(e1p, e2p);
  }
  virtual bool isConst()
  {
    return false;
  }
  virtual int getConst()
  {
    return 0;
  }
  virtual string toString()
  {
    ostringstream oss;
    oss << "Produs(" << e1->toString() << ", " << e2->toString() << ")";
    return oss.str();
  }
};

class ExpoExpr : public Expr
{
private:
  Expr *e1;
  Expr *e2;
public:
  ExpoExpr(Expr *e1, Expr *e2) : e1(e1), e2(e2) {}
  virtual int eval()
  {
    return 0; // todo
  }
  virtual Expr *derivata()
  {
    // todo
  }
  virtual Expr *simpl()
  {
    Expr *e1p = e1->simpl();
    Expr *e2p = e2->simpl();
    if (e1p->isConst() && e2p->isConst()) {
      return new ConstExpr(e1p->getConst() *
                           e2p->getConst());
    }
    if (e1p->isConst() && e1p->getConst() == 0) {
      return new ConstExpr(0);
    }
    if (e2p->isConst() && e2p->getConst() == 0) {
      return new ConstExpr(0);
    }
    if (e1p->isConst() && e1p->getConst() == 1) {
      return e2p;
    }
    if (e2p->isConst() && e2p->getConst() == 1) {
      return e1p;
    }
    return new ProdusExpr(e1p, e2p);
  }
  virtual bool isConst()
  {
    return false;
  }
  virtual int getConst()
  {
    return 0;
  }
  virtual string toString()
  {
    ostringstream oss;
    oss << "Produs(" << e1->toString() << ", " << e2->toString() << ")";
    return oss.str();
  }
};

int main()
{
  Expr *e1 = new SumaExpr(new ConstExpr(7), new ConstExpr(14));
  Expr *e2 = new ProdusExpr(new ConstExpr(3),
                            new SumaExpr(
                                         new ConstExpr(7),
                                         new ConstExpr(14)));
  Expr *e5 = new SumaExpr(
                          new ProdusExpr(new ConstExpr(3),
                                         new ProdusExpr(new VarExpr("x"), new VarExpr("x"))), new ConstExpr(7));
  cout << e1->eval() << endl;
  cout << e2->eval() << endl;
  cout << e5->derivata()->toString() << endl;
  cout << e5->derivata()->simpl()->toString() << endl;
  return 0;
}
