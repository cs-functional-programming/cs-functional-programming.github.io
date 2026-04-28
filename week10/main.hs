import System.IO
import System.Environment
import Control.Exception

-- Monada IO (si despre alte monade)

-- daca programul contine o actiune IO numita "main" sistemul
-- evalueaza functia main si executa actiunea intoarsa de functia main

-- Exista in Haskell un tip de date "IO a", parametrizat de o
-- variabila de tip "a", iar valorile de tip "IO a" sunt _actiuni IO_
-- (comenzi IO)

-- O actiune de tip "IO a" produce, la sfarsitul executiei, o valoare
-- de tip "a".

-- actiunea produce o valoare
-- functie returneaza o valoare

f :: () -> ()
f x = x

main1 :: IO ()
main1 = putStrLn "Hello, World!"

main2 :: IO ()
main2 = (putStrLn "Hello, World!") >> (putStrLn "Hello, X!")

main3 :: IO ()
main3 = putStrLn "Hello, World!" >>
        putStrLn "Hello, X!" >>
        putStrLn "Bye!"

main4 :: IO ()
main4 = putStrLn "Hello, World!" >>
        putStr "What is your name? ">>
        getLine >>
        putStrLn "Bye!"

main5 :: IO ()
main5 = putStrLn "Hello, World!" >>
        putStr "What is your name? ">>
        getLine >>=
        (\name -> putStrLn $ "Bye, " ++ name ++ "!")

main6 :: IO ()
main6 = do putStrLn "Hello, World!"
           putStr "What is your name? "
           name <- getLine
           putStrLn $ "V6: Bye, " ++ name ++ "!"

main7 :: IO ()
main7 = do putStrLn "Hello, World!"
           putStr "What is your name? "
           getLine >>= \name -> putStrLn $ "V7: Bye, " ++ name ++ "!"

main8 :: IO ()
main8 = do {
  putStrLn "Hello, World!";
  putStr "What is your name? ";
  name <- getLine;
  putStrLn $ "V8: Bye, " ++ name ++ "!"
  }

main9 :: IO ()
main9 = do putStrLn "Hello, World!"
           putStr "What is your name? "
           name <- getLine -- Atentie: nu are deloc acelasi rol cu "="
           putStrLn $ "V9: Bye, " ++ name ++ "!"

-- main9 :: IO ()
-- syntactic sugar pentru
-- main9 = putStrLn "Hello, World!" >>
--           (putStr "What is your name? " >>
--           (getLine >>= (\name -> -- Atentie: nu are deloc acelasi rol cu "="
--           putStrLn $ "V9: Bye, " ++ name ++ "!")))

main10 :: IO ()
main10 = do putStrLn "Hello, World!"
            putStr "What is your name? "
            let name = getLine in do
              x <- name
              putStrLn $ "V10: Bye, " ++ x ++ "!"

main11 :: IO ()
main11 = do putStr "What is your name? "
            name <- getLine
            if name == "" then
              return ()
            else do putStrLn $ "Hello, " ++ name ++ "!"
                    main11

main12 :: IO ()
main12 = do putStr "What is your name? "
            name <- getLine
            return () -- nu seamana cu return-ul din C/C++/Java
                      -- in sensul in care nu afecteaza
                      -- control flow-ul programului
                      -- "return x" e o actiune care,
                      -- la executie, nu face nimic,
                      -- dar produce la sfarsit valoarea "x"
            putStrLn $ "Hello, " ++ name ++ "!"

-- main :: IO ()
-- main = do args <- getArgs
--           name <- getProgName
--           putStrLn $ show args
--           putStrLn $ "Fisierul este: " ++ name

-- greseala frecventa (!)

-- nu se poate scrie in Haskell o functie
-- transform :: IO String -> String

-- main :: IO ()
-- main = do args <- getArgs
--           putStrLn $ show args
--           putStrLn $ "Fisierul este: " ++ getProgName

-- Monada IO este un pic ca un wrapper pentru programe de facto
-- imperative.

-- Daca o functie imi intoarce o valoare de tip "IO a"
-- nu pot sa extrag intr-un program valoarea "a" decat
-- daca sunt in continuare sub monada IO.

-- nu exista o functie putStrLn' -> String -> ()

-- un program Haskell mai mare va fi impartit in doua parti:
-- 1. functii pure
-- 2. functii care intorc actiuni IO
-- Prefer cat mai multe functii pure si cat mai putine functii IO.

-- Monada IO asigura ca nu pot contamina o functie pura cu efecte din
-- monada IO.

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [] -> putStrLn "Syntax: main <filename>"
--             (fname:_) -> do handle <- openFile fname ReadMode
--                             contents <- hGetContents handle
--                             putStr contents

isComment :: String -> Bool
isComment ('-':'-':_) = True
isComment _ = False

notIsComment :: String -> Bool
notIsComment s = not (isComment s)

process :: String -> String
process s = unlines $ filter notIsComment (lines s)

-- process :: Handle -> IO ()
-- process handle = do line <- hGetLine handle
--                     if not (isComment line) then
--                       putStrLn line
--                     else
--                       return ()
--                     if line == "--DONE" then
--                       return ()
--                     else
--                       process handle

mainA :: IO ()
mainA = do args <- getArgs
           case args of
            [] -> putStrLn "Syntax: main <filename>"
            (fname:_) -> do handle <- openFile fname ReadMode
                            contents <- hGetContents handle
                            putStr $ process contents
                            
--DONE

handleExc :: IOException -> IO ()
handleExc e = putStrLn $ "A aparut o exceptie" ++ (show e)

mainB :: IO ()
mainB = do args <- getArgs
           case args of
            [] -> putStrLn "Syntax: main <filename>"
            (fname:_) -> do handle <- openFile fname ReadMode
                            contents <- hGetContents handle
                            putStr contents
                         `catch`
                         handleExc

handler' :: ArithException -> IO ()
handler' e = putStrLn $ "Hopa: " ++ (show e)

main :: IO ()
main = do putStrLn "Hello"
          let x = 10 `div` 0 in
            putStrLn $ "Result is " ++ (show x)
       `catch`
       handler'

-- Maybe este tot o monada

impartire :: Integer -> Integer -> Maybe Integer
impartire _ 0 = Nothing
impartire x y = Just $ div x y

impartireLantA :: Integer -> Integer -> Integer -> Maybe Integer
impartireLantA _ 0 _ = Nothing
impartireLantA _ _ 0 = Nothing
impartireLantA x y z = Just $ div (div x y) z

--impartireLantB :: Integer -> Integer -> Integer -> Maybe Integer
--impartireLantB x y z = impartire (impartire x y) z

impartireLantC :: Integer -> Integer -> Integer -> Maybe Integer
impartireLantC x y z = case impartire x y of
                         Nothing -> Nothing
                         Just r -> impartire r z

impartireLantD :: Integer -> Integer -> Integer -> Maybe Integer
impartireLantD x y z = (impartire x y) >>= (\r -> impartire r z)

impartireLantE :: Integer -> Integer -> Integer -> Maybe Integer
impartireLantE x y z = do r <- impartire x y
                          impartire r z

-- How to replace a failure by a list of successes (Philip Wadler, 1985)

impartire' :: Integer -> Integer -> [Integer]
impartire' _ 0 = []
impartire' x y = [ div x y ]

impartireLantF :: Integer -> Integer -> Integer -> [Integer]
impartireLantF x y z = do r <- impartire' x y
                          impartire' r z

succesor :: String -> [String]
succesor s = [ s ++ "0", s ++ "1" ]

posibilitati :: [String]
posibilitati = do x <- succesor ""
                  y <- succesor x
                  z <- succesor y
                  return z

backtracking :: Int -> [String] -> [String]
backtracking 0 l = l
backtracking n l = backtracking (n - 1) l >>= succesor
