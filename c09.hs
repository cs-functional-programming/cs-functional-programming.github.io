{-

PART I: Module

1. GHC: numele unui modul = numele fisierului (in Haskell ca limbaj, nu neparat)

2. Organizez modulele intr-o ierarhie de directoare

modulul din fisierul D1/D2/Nat.hs trebuie sa se cheme D1.D2.Nat

3. Numele modulului trebuie sa inceapa cu o litera mare

4. importurile trebuie sa fie la inceputul unui modular

5. importuri "qualified"
6. importuri "hiding"
7. exporturi cu "(..)"

Haskell face parte dintr-o familia "ML" (impreuna cu OCaml, Standard ML, ML/NJ, MLton, Miranda, F#,
...)

PART II: Instrumentul "stack" e un fel de Maven

Scopul utilitarului "stack" este 
- sa am "reproducible builds".
- proiectul meu sa fie independent de ghc-ul instalat/pachetele instalate la nivelul sistemului.        

Cabal este 1) package managerul "standard" pentru Haskell
           2) specificatia pentru pachete

Comenzi:

stack new <project-name> --> creaza un proiect implict
stack build              --> compileaza proiectul
stack run                --> ruleaza executabilul
stack install            --> copiaza executabilul in locatia sistem predefinita

TODO LABORATOR:
-- utilizati stack pentru a va crea un proiect in care sa declarati ca librarii Nat, Int si sa le folositi din Main

-- veti scrie functii
convert :: Nat -> Int
convert' :: Int -> Nat

-- veti folosi PBT ca sa verificati ca convert . convert' = id
                                       convert' . convert = id
-- adaugati cele doua proprietati la suita de test ale proiectului (ca sa se testeze cu "stack test")
-- ca add pentru Nat se comporta la fel ca + peste Int

PART III: Property-Based Testing

-}

module Main where

-- import Prelude hiding (length)
import qualified Utils.Nat as N
import qualified Utils.Int as I
import Test.QuickCheck
import Data.List

import qualified Utils.Str

qs''' :: [Int] -> [Int]
qs''' [] = []
qs''' (hd:tl) = (qs''' (filter (<=hd) tl)) ++ [hd] ++ (qs''' (filter (>hd) tl))

qs' :: [Int] -> [Int]
qs' [] = []
qs' (hd:tl) = (filter (<hd) tl) ++ [hd] ++ (filter (>hd) tl)

qs :: [Int] -> [Int]
qs [] = []
qs (hd:tl) = (qs $ filter (<hd) tl) ++ [hd] ++ (qs $ filter (>hd) tl)

sorted :: [Int] -> Bool
sorted [] = True
sorted [x] = True
sorted (hd:hd':tl) = (hd <= hd') && sorted (hd':tl)

prop1 :: [Int] -> Bool
prop1 l = sorted (qs' l)

prop2 :: ([Int] -> [Int]) -> [Int] -> Bool
prop2 sortfun l = sorted (sortfun l)

prop3 :: ([Int] -> [Int]) -> [Int] -> Bool
prop3 sortfun l = length l == length (sortfun l)

-- o functie de "sortare" care respecta prop2 si prop3 si totusi nu sorteaza?

qs'' :: [Int] -> [Int]
qs'' = (map (+1)) . qs

prop4 :: ([Int] -> [Int]) -> (Int, [Int]) -> Bool
prop4 sortfun (x, l) = (x `elem` l) == (x `elem` sortfun l)

prop5 :: ([Int] -> [Int]) -> [Int] -> Bool
prop5 sortfun l = sort l == sortfun l

-- nu e de dorit asa:
prop6 :: Int -> Int -> Bool
prop6 n x = (n <= 0) || (replicate n x) !! 0 == x
-- fiindca imi reduce acoperirea

-- prop7 :: Int -> Int -> Bool
prop7 n x = (n > 0) ==> (replicate n x) !! 0 == x

main :: IO ()
main = do
     putStrLn ("Hello, World! " ++ (show (N.add N.doi N.trei)))
     putStrLn ("Hihi! " ++ (show (N.add N.Zero (N.Succ N.trei))))
     putStrLn ("Hello, World! " ++ (show (I.add (I.Cons 2) (I.Cons 3))))
--     putStrLn (show $ length (Cons "asdf"))
