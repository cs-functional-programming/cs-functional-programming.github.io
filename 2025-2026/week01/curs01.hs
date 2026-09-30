-- Mod interactiv / REPL
-- E f. usor de experimentat cu limbajul
-- in modul interactiv.
-- sa testez o functie...

-- ghilimele: "
-- apostrof:  '
-- backtick:  `
-- backslash: \

-- 10 `div` 3
---  ^^^
-- nu e apostrof, e BACKTICK
-- il gasiti langa cifra 1 pe tastatura

-- SINTAXA DE APEL A UNEI FUNCTII IN HASKELL
---- numele functiei
---- spatiu
---- parametri, separati prin " "

-- SINTAXA DE APEL A UNEI FUNCTII IN C/C++
---- numele functiei
---- paratenza deschisa
---- parametri, separati prin ","
---- paranteza inchisa
---- Exemplu: f(2, 3)

--- Daca functia are un singur argument:
---   Nu e gresit sintactic sa scriu "f(2)"
---   Dar e preferabil sa scriu      "f 2"
---                                  "f (f 2)"


-- ZAHAR SINTACTIC: in loc de "f x y", pot scrie "x `f` y"

-- Cum declar o variabila
a :: Integer
a = 10

b :: String
b = "asdf20"

-- Cum declar o functie
minus :: Integer -> Integer
-- cum definesc o functie
-- functiile se definesc prin ecuatii in Haskell
minus x = 0 - x

suma :: Integer -> Integer -> Integer
suma a b = a + b

max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = a `max` b `max` c

-- Modul interactiv favorizeaza dezvoltarea de programe
-- intr-un stil bottom-up

{-

int max2(int a, int b)
{
  if (a > b) { return a; } else { return b; } // aici e o instructiune
}

int max2(int a, int b)
{
  return
  a > b ? a : b; // aici este o expresie
}

int max2(int a, int b)
{
  int v = a > b ? a : b;
}
  
-}

max2 :: Integer -> Integer -> Integer
max2 a b = if a > b then a else b

-- eroare de sintaxa sa lipseasca else sau then
--max2' :: Integer -> Integer -> Integer
--max2' a b = if a > b then a

--max3' :: Integer -> Integer -> Integer -> Integer
--max3' a b c = if

{-

int sumUpTo(int n) // pp. n >= 0
{
  int s = 0;
  for (int i = 0; i <= n; ++i) {
    s += i;
  }
  return s;
}

-}

sumUpTo :: Integer -> Integer
sumUpTo 0 = 0
sumUpTo n = n + (sumUpTo (n - 1))

--sumUpTo' :: Integer -> Integer
--sumUpTo' n = n + (sumUpTo' (n - 1))
--sumUpTo' 0 = 0


-- cum se evalueaza un apel de functie daca sunt mai multe
-- ecuatii pentru functia respectiva

{-

bool isPrime(int n) // pp n >= 2
{
  for (int i = 2; i <= n - 1; ++i) {
    if (n % i == 0) {
      return false;
    }
  }
  return true;
}

-}

-- isPrimeAux :: Integer -> Integer -> Bool
-- isPrimeAux n i = if i > n - 1 then
--                    True
--                  else if n `mod` i == 0 then
--                    False
--                  else
--                    isPrimeAux n (i + 1)

-- isPrime :: Integer -> Bool
-- isPrime n = isPrimeAux n 2

hasDivisorsBetween :: Integer -> Integer -> Integer -> Bool
hasDivisorsBetween n a b = if a > b then
                             False
                           else if n `mod` a == 0 then
                             True
                           else
                             hasDivisorsBetween n (a + 1) b

isPrime :: Integer -> Bool
isPrime n = not (hasDivisorsBetween n 2 (n - 1))
