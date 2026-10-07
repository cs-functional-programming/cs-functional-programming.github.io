-- limbaje pur funcționale:
--   nu există efecte secundare
--   nu există instrucțiuni
--   avem doar expresii
-- dacă apare de două ori aceeași
-- expresie într-un program,
-- indiferent de contextul în care
-- apare expresia,
-- expresia are aceeași valoare
-- de ambele dăți

{-
sigur ambele "g x"-uri au aceeași
valoare
                vvv
f x y = let z = g x in
        let z' = g x in
                 ^^^
          z + z' + y

transparență referențială =
două apariții ale aceleiași expresii
au ambele aceeași valoare
-}

-- cum calculez în Haskell

{-
-- cum calculez în C++/Java/C:
---- declar variabile      (da, pot să declar în Haskell
                            variabile, doar că nu pot
                            să mă răzgândesc)
     int x;

---- folosesc instrucțiunea de atribuire
           (nu avem instrucțiunea de atribuire în Haskell)
     x = x + 7;

---- structura condițională    (da, oarecum)
                               (în Haskell este o expresie
                                condițională, nu o instrucțiune)
                                
     if (x % 2 == 0) {
       x++;
     } else {
       x = x + 2;
     }

---- afișare (nu e propriu-zis calcul)
---- input (nu e propriu-zis calcul)
---- includem header-e (nu e fundamental)

---- declarăm funcții       (da, se poate și în Haskell)
                            (nu pot avea efecte secundare)
     int is_prime(int x)
     {
       for (int i = 0; i < 100; ++i)
     }

                                NU           NU
                                vvvvv        vvvvvv
---- structuri repetitive (for, while, goto, setjmp, funcții recursive ...)
                           ^^^         ^^^^          ^^^^^^^^^^^^^^^^^
                           NU          NU                   DA
-}


is_even_0 x = if x `mod` 2 == 0 then
                True
              else
                False
{---
ghci> is_even_0 13.3
is_even_0 13.3
<interactive>:4:1: error:
...
Eroarea a apărut înainte să fie evaluată expresia.
Haskell este un limbaj tipizat static (statically typed).
Python este un limbaj tipizat dinamic (erorile de tip sunt,
  în general, identificate pe parcursul execuției)
Nu prea mai există limbaje netipizate (nici static, nici dinamic).
Limbajele netipizate (variante vechi de BASIC):
10 PRINT "HELLO"
20 GOTO 10
weird things: "7" + 3 -> "10"

---}

-- Haskell face "inferență de tipuri"
-- Dacă nu declar tipul unui identificator,
-- does its best să îl găsească.

-- Good practice: să declar tipurile tuturor funcțiilor
-- definite la top-level.
is_even_1 :: Int -> Bool
is_even_1 x = if x `mod` 2 == 0 then
                True
              else
                False


is_even_2 :: Int -> Bool
is_even_2 x = x `mod` 2 == 0

{-
int main()
{
  int x = 13;
  while (x > 1) {
    x -= 2;
  }
  if (x == 0) {
    cout << "Par" << endl;
  } else {
    cout << "ImPar" << endl;
  }
}

-}

is_even_3 :: Int -> Bool
is_even_3 x = if x >= 2 then
                is_even_3 (x - 2)
              else if x == 0 then
                True
              else
                False

is_even_4 :: Int -> Bool
is_even_4 x = if x >= 2 then
                is_even_4 (x - 2)
              else
                x == 0

is_even_5 :: Int -> Bool     --     <--- preferabil in dauna
is_even_5 0 = True           -- folosii expresiei if-then-else
is_even_5 1 = False
is_even_5 x = is_even_5 (x - 2)
-- mod de evaluare: merg de sus in jos in fisier
-- pana la prima ecuatie care se potriveste

is_even_6 :: Int -> Bool
is_even_6 x = is_even_6 (x - 2) -- atentie: infinite loop
--is_even_6 0 = True        -- 0 si 1 sunt "redundant"     
--is_even_6 1 = False


is_even_7 :: Int -> Bool
is_even_7 x | x <= 1 = x == 0  -- ecuatie conditionala
--            ^^^^^^
--            "garda" ecuatiei (guard)
is_even_7 x          = is_even_7 (x - 2)

{-int is_prime(int x) // is_prime(1) = true in mod gresit
{
  for (int i = 2; i <= x / 2; ++i) {
    if (x % i == 0) {
      return false; // (*)
    }
  }
  return true; // (**)
}-}

-- is_prime_aux :: Int -> Int -> Bool // nu merge bine pentru x = 2
-- is_prime_aux x i = if x `mod` i == 0 then
--                      False -- (*) 
--                    else if i > x `div` 2 then
--                      True -- (**) am ieșit din "for"
--                    else
--                      is_prime_aux x (i + 1)
                     
-- is_prime :: Int -> Bool
-- is_prime x = is_prime_aux x 2
is_prime_aux :: Int -> Int -> Bool
is_prime_aux x i = if i > x `div` 2 then
                     True -- (**) am ieșit din "for"
                   else if x `mod` i == 0 then
                     False -- (*) 
                   else
                     is_prime_aux x (i + 1)
                     
is_prime :: Int -> Bool
is_prime x = is_prime_aux x 2

-- genul acesta de traducere din C in Haskell este contraproductivă

-- x este prim ddaca nu are divizori proprii
-- x este prim ddaca nu are divizori intre 2 si x / 2

-- "calculeaza daca x nu are divizori intregi in intervalul [a,b]"
hasNoDivisorsBetween :: Int -> Int -> Int -> Bool
hasNoDivisorsBetween x a b = if a > b then
                               True
                             else if x `mod` a == 0 then
                               False
                             else
                               hasNoDivisorsBetween x (a + 1) b

is_prime' :: Int -> Bool
is_prime' x = hasNoDivisorsBetween x 2 (x `div` 2)

hasDivisorsBetween :: Int -> Int -> Int -> Bool
hasDivisorsBetween x a b = if a > b then
                               False
                             else if x `mod` a == 0 then
                               True
                             else
                               hasDivisorsBetween x (a + 1) b

is_prime'' :: Int -> Bool
is_prime'' x = not (hasDivisorsBetween x 2 (x `div` 2))

hasDivisorsAbove :: Int -> Int -> Bool
-- hasDivisorsAbove x (x `div` 2) = False
--                    ^^^^^^^^^^^
--                nu sunt permise apeluri de functie intr-un pattern
hasDivisorsAbove x a = if a > (x `div` 2) then
                         False
                       else if x `mod` a == 0 then
                         True
                       else
                         hasDivisorsAbove x (a + 1)

is_prime''' :: Int -> Bool
is_prime''' x = not (hasDivisorsAbove x 2)
