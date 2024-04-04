import System.Environment
import System.IO
import Control.Exception

-- Monada IO
-- = modalitatea prin care pot scrie
-- de facto programe imperative in Haskell (e.g., citesc un fisier,
-- trimit un mesaj in retea, s.a.m.d.)

-- IO a este un tip parametrizat de variabila de tip "a"

-- Valorile de tip "IO a" sunt actiuni/instructiuni/comenzi, pe care,
-- daca le execut, la sfarsit vor produce o valoare de tip "a".

-- Spun "produce" si nu "returneaza" fiindca pastrez verbul returneaza
-- pentru functii.

-- Tipul unit: ()
-- Valoarea unit: ()
-- valoarea unit este singura valoare de tip unit

-- main :: IO ()
-- main = putStrLn "Hello, World!"

-- 1. se va evalua functia numita "main" (practic, nu se intampla nimic)
-- 2. se executa actiunea intoarsa de functia "main"


-- Am doua actiuni: act1 si act2.
-- Vreau sa creez o noua actiune care sa execute intai act1 si apoi act2.

-- main :: IO ()
-- main = (>>) (putStrLn "Hello, World!")
--             (putStrLn "I am GHC.")

-- main :: IO ()
-- main = putStrLn "Hello, World!" >>
--        putStrLn "I am GHC." >>
--        putStrLn "I am done."


-- putStrLn s intoarce/returneaza o actiune IO care, daca e executata,
-- afiseaza s la iesirea standard si produce apoi valoarea ().

-- getLine :: IO String
-- getLine nu primeste niciun argument si intoarce o actiune

-- Cand execut actiunea, sistemul asteapta de la tastatura un sir de
-- caractere si apoi (dupa ce apas enter) se produce ca rezutat sirul.

-- main :: IO ()
-- main = putStrLn "What is your name?" >>
--        getLine >> -- cum "pun mana" pe sirul produs de
--                   -- actiunea intoarsa de getLine?
--        putStrLn "Hello!"

-- (>>=) :: IO a -> (a -> IO b) -> IO b

-- main :: IO ()
-- main = putStrLn "What is your name?" >>
--        getLine >>=
--        (\x -> putStrLn $ "Hello, " ++ x ++ "!")

-- Pentru monade, Haskell defineste un zahar sintactic
-- prin "notatia do" ("do" notation)

-- main :: IO ()
-- main = do putStrLn "Hello, World!"
--           putStrLn "I am GHC."
--           putStrLn "I am done."
          
-- Atentie la indentare! Daca actiunile dintr-o notatie "do" nu sunt
-- aliniate, apare o eroare la parsare.

-- main :: IO ()
-- main = do { putStrLn "Hello, World!" ;
--               putStrLn "I am GHC." ;
--            putStrLn "I am done." }

-- main :: IO ()
-- main = do
--          putStrLn "Hello, World!"
--          putStrLn "I am GHC."
--          putStrLn "I am done."

-- Cum folosim notatia "do" daca vrem "sa punem mana" pe valoarea produsa
-- de una dintre actiuni?

-- main :: IO ()
-- main = do putStrLn "What is your name?"
--           x <- getLine
--           putStrLn $ "Hello, " ++ x ++ "!"

-- Cum "dezahariseste" sistemul notatia "do"
-- main = (putStrLn "What is your name?" >>
--            (getLine >>= (\x ->
--              putStrLn $ "Hello, " ++ x ++ "!")))


-- Atentie! A nu se confunda "x <- getLine" cu "let x = getLine"

-- <- nu este o functie, face parte din notatia "do"

-- main :: IO ()
-- main = do putStrLn "What is your name?"
--           let x = getLine in do
--             y <- x
--             putStrLn $ "Hello, " ++ y ++ "!"

-- Cum obtin un comportament repetitiv?

-- main :: IO ()
-- main = do putStrLn "What is your name?"
--           name <- getLine
--           putStrLn $ "Hello, " ++ name ++ "!"
--           main

-- main :: IO ()
-- main = do putStrLn "What is your name?"
--           name <- getLine
--           if name == "" then
--             putStrLn "Done!"
--           else
--             do putStrLn $ "Hello, " ++ name ++ "!"
--                main

-- return :: a -> IO a
-- return x intoarce o actiune care, daca este executata,
-- nu face absolut nimic, dar la sfarsit produce valoarea x

-- main :: IO ()
-- main = do putStrLn "What is your name?"
--           name <- getLine
--           if name == "" then
--             return ()
--           else
--             do putStrLn $ "Hello, " ++ name ++ "!"
--                main

-- Atentie! return nu seamana deloc cu "return"-ul din C/C++/Java/Python

-- return in Haskell e doar o functie care intoarce o actiune no-op

-- return in C/C++/Java/Python e o instructiune care schimba control
-- flow-ul programului

-- main :: IO ()
-- main = do return ()
--           putStrLn "Hello, World!"


-- (G, +, ...) e grup ddaca ...

-- (M, >>=, return) e monada ddaca ...

-- IO se intampla sa satisfaca aceste proprietati

-- La sfarsit sa va arat cum pot transforma un esec intr-o lista de
-- succesuri :)

-- main :: IO ()
-- main = do args <- getArgs
--           putStrLn $ show args

-- putStr :: String -> IO ()

-- Eroare frecventa:
-- main :: IO ()
-- main = do args <- getArgs
--           putStrLn $ show args
--           putStrLn $ "Sunt programul " ++ getProgName
--                      ^^^^^^^^^^^^^^^^^    ^^^^^^^^^^^
--                          String              IO String
--          (++) :: String -> String -> String

-- main :: IO ()
-- main = do args <- getArgs
--           putStrLn $ show args
--           progName <- getProgName
--           putStrLn $ "Sunt programul " ++ progName

-- Am functia getProgName :: IO String.

-- Vreau sa imi fac functia getProgName' :: String pornind de la
-- getProgName.

-- Cum procedez? Nu se poate.

-- La fel pentru getLine :: IO String.

{-

  Daca o functie Haskell intoarce o valoare de tip IO a, functia este
de facto imperativa.

  Monada IO protejeaza toate functiile "de facto" imperative.

  Un program Haskell mai mare va fi impartit in:

  1. functii pure
  2. functii din monada IO

  Prefer sa am cat mai multe functii pure si cat mai putine functii
imperative.

  Monada IO asigura faptul ca nu risc sa contaminez o functie pura cu
un apel imperativ.

-}

-- x = case [1,2] of
--          [] -> "vida"
--          (hd:tl) -> "asdf"

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [] -> putStrLn "Dati un fisier ca argument!"
--             (hd:_) -> do handle <- openFile hd ReadMode
--                          contents <- hGetContents handle
--                          putStrLn contents

isComment :: String -> Bool
isComment ('-' : '-' : _) = True
isComment _ = False

notIsComment :: String -> Bool
notIsComment = \line -> not (isComment line)

proceseaza :: String -> String
proceseaza contents = unlines $ filter notIsComment (lines contents)

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [] -> putStrLn "Dati un fisier ca argument!"
--             (hd:_) -> do handle <- openFile hd ReadMode
--                          contents <- hGetContents handle
--                          putStrLn $ proceseaza contents

-- catch :: Exception e => IO a -> (e -> IO a) -> IO a

handler :: IOException -> IO ()
handler e = putStrLn "A aparut o exceptie!"

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [] -> putStrLn "Dati un fisier ca argument!"
--             (hd:_) -> catch (do handle <- openFile hd ReadMode
--                                 contents <- hGetContents handle
--                                 putStrLn $ proceseaza contents)
--                             handler
                            
-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [] -> putStrLn "Dati un fisier ca argument!"
--             (hd:_) -> do handle <- openFile hd ReadMode
--                          contents <- hGetContents handle
--                          putStrLn $ proceseaza contents
--                       `catch`
--                       handler

handler' :: ArithException -> IO ()
handler' _ = putStrLn "Eroare!"

main :: IO ()
main = (putStrLn $ show (div 10 0))
       `catch`
       handler'


-- "Turning a failure into a list of successes"

-- Monada Maybe

returnMaybe :: a -> Maybe a
returnMaybe x = return x

-- >>=

seqMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
seqMaybe = (>>=)

v1 = Nothing :: Maybe Int
v2 = Just 10 :: Maybe Int
v3 = Just 0 :: Maybe Int

w1 :: Int -> Maybe Int
w1 x = if x == 0 then Nothing else Just (div 10 x)

impartire''' :: Integer -> Integer -> Maybe Integer
impartire''' x y = if y == 0 then
                     Nothing
                   else
                     Just (x `div` y)

impartireInLant :: Integer -> Integer -> Integer -> Maybe Integer
impartireInLant x y z = impartire''' x y >>= (\a -> impartire''' a z)
                      -- = case impartire''' x y of
                      --     Nothing -> Nothing
                      --     Just r -> impartire''' r z

returnList :: a -> [a]
returnList x = return x

seqList :: [a] -> (a -> [b]) -> [b]
seqList = (>>=)

f x = [1..x]

-- aplicati un map si apoi un concat: candidat bun pentru >>=


-- esec = lista vida de succesuri :)

impartire :: Integer -> Integer -> [Integer]
impartire x y = if y == 0 then
                     []
                else
                     [(x `div` y)]

impartireInLant' :: Integer -> Integer -> Integer -> [Integer]
impartireInLant' x y z = impartire x y >>= (\a -> impartire a z)
