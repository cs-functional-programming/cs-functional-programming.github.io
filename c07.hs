import System.Environment
import System.IO
import Control.Exception

-- Tipul "unit"

-- data () = ()

f :: () -> Int
f () = 13

-- Monada IO

-- "IO a" = actiuni IO/comenzi IO/instructiuni
--          in urma executiei unei astfel de actiuni, se produce
--                                                       ^^^^^^^
--                                                 nu returneaza/intoarce
--          o valoare de tip a.

-- main :: IO ()
-- main = putStrLn "Hello, World!"

-- main :: IO ()
-- main = (>>) (putStrLn "Hello, World!") (putStrLn "My name is.")

-- main :: IO ()
-- main = putStrLn "Hello, World!" >> putStrLn "My name is."

-- main :: IO ()
-- main = putStrLn "Hello!" >>
--        putStrLn "My name is." >>
--        putStrLn "Bye!"

g1 :: IO ()
g1 = putStrLn "What is your name?" >>
     getLine >>
     putStrLn "Hello!"

g2 :: IO ()
g2 = putStrLn "What is your name?" >>
     getLine >>=
     (\name -> putStrLn ("Hello, " ++ name ++ "!"))

-- notatia "do"

g3 :: IO ()
g3 = do putStrLn "What is your name?"
        name <- getLine
        putStrLn ("Hello, " ++ name ++ "!")

-- greseala frecventa
-- g4 :: IO ()
-- g4 = do putStrLn "What is your name?"
--         let name = getLine in
--           putStrLn ("Hello, " ++ name ++ "!")

-- atentie la indentare la notatia "do"
-- g5 :: IO ()
-- g5 = do putStrLn "What is your name?"
--           name <- getLine
--       putStrLn ("Hello, " ++ name ++ "!")

g6 :: IO ()
g6 = do { putStrLn "What is your name?";
      name <- getLine;
       putStrLn ("Hello, " ++ name ++ "!") }

{-
In loc de:
     do putStrLn "What is your name?"
        name <- getLine
        putStrLn ("Hello, " ++ name ++ "!")
sistemul intelege:
  putStrLn "What is your name?" >>
  getLine >>= (\name ->
        putStrLn ("Hello, " ++ name ++ "!")
  )
-}

-- Atentie!  "<-" nu e o functie, ci face parte din notatia "do"

g7 :: IO ()
g7 = do putStrLn "What is your name?"
        name <- getLine
        if name /= "" then
          do putStrLn $ "Hello, " ++ name ++ "!"
             g7
        else
          putStrLn "Bye!"

g8 :: IO ()
g8 = do putStrLn "What is your name?"
        name <- getLine
        if name /= "" then
          do putStrLn $ "Hello, " ++ name ++ "!"
             g7
        else
          return ()

-- Atentie! return este o functie, nu o metoda de modificare a fluxului de control

g9 :: IO ()
g9 = do putStrLn "Hello, "
        return ()
        putStrLn "World!"

-- (G, +, i, e) este grup ddaca
--      (+) este asociativa: a + (b + c) == (a + b) + c
--      e este elementul identitate: ...

-- IO a este o monada
-- (M, >>=, return) este o monada daca:
--   1) >>= este "asociativ"
--   2) return este "identitatea" la stanga si dreapta pentru >>=

{-
int main(int argc, char **argv)
{
}
-}

-- main :: IO ()
-- main = do x <- getArgs
--           y <- getProgName
--           putStrLn $ "Sunt executabilul " ++ y
--           putStrLn $ "Argumentele mele sunt: " ++ show x

-- O greseala frecventa
-- -- imposibil:
-- unIO :: IO String -> String
-- unIO = ...
-- main :: IO ()
-- main = do x <- getArgs
--           -- y <- getProgName
--           putStrLn $ "Sunt executabilul " ++ (unIO getProgName)
--           putStrLn $ "Argumentele mele sunt: " ++ show x

-- Un program Haskell contine:
--        1) functii "pure"
--        2) functii "IO
-- E preferabil sa am cat mai multe functii pure in dauna celor IO
-- De ce? E imposibil de "scapat" de sub monada IO

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [] -> putStrLn "Dati numele fisierului in linia de comanda"
--             (fname:_) -> putStrLn $ "Proceseaza fisierul " ++ fname

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [] -> putStrLn "Dati numele fisierului in linia de comanda"
--             (fname:_) -> do
--               hFile <- openFile fname ReadMode
--               contents <- hGetContents hFile
--               putStrLn contents

-- proceseaza :: Handle -> IO ()
-- proceseaza hFile = do line <- hGetLine hFile
--                       case line of
--                         [] -> putStrLn ""
--                         [hd] -> putStrLn [hd]
--                         (c1:c2:tl) -> if c1 == c2 && c1 == '-' then
--                                         return ()
--                                       else
--                                         putStrLn line
--                       catch
--                         (proceseaza hFile)
--                         (\(e :: IOException) -> putStrLn "DONE!")

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [] -> putStrLn "Dati numele fisierului in linia de comanda"
--             (fname:_) -> do
--               hFile <- openFile fname ReadMode
--               proceseaza hFile

-- main :: IO ()
-- main = do x <- return (10 `div` 0)
--           putStrLn $ "Rezultatul este " ++ show x
--           `catch`
--           (\(e :: ArithException) -> putStrLn "Trouble!")

-- isComment :: String -> Bool
-- isComment [] = False
-- isComment [_] = False
-- isComment (c1:c2:tl) = c1 == '-' && c2 == '-'
  
-- proceseazaLinie :: String -> IO ()
-- proceseazaLinie line = if isComment line then
--                          return ()
--                        else
--                          putStrLn line

-- proceseaza :: Handle -> IO ()
-- proceseaza hFile = do line <- hGetLine hFile
--                       proceseazaLinie line
--                       catch
--                         (proceseaza hFile)
--                         (\(e :: IOException) -> putStrLn "DONE!")

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [] -> putStrLn "Dati numele fisierului in linia de comanda"
--             (fname:_) -> do
--               hFile <- openFile fname ReadMode
--               proceseaza hFile


-- isComment :: String -> Bool
-- isComment [] = False
-- isComment [_] = False
-- isComment (c1:c2:tl) = c1 == '-' && c2 == '-'
  
-- proceseazaLines :: [String] -> [String]
-- -- proceseazaLines lines = filter (\line -> not (isComment line)) lines
-- -- proceseazaLines lines = filter (not . isComment) lines
-- proceseazaLines = filter (not . isComment)

-- proceseazaContents :: String -> String
-- proceseazaContents contents = unlines (proceseazaLines (lines contents))

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [] -> putStrLn "Dati numele fisierului in linia de comanda"
--             (fname:_) -> do
--               hFile <- openFile fname ReadMode
--               contents <- hGetContents hFile
--               putStr $ proceseazaContents contents

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [] -> putStrLn "Dati numele fisierului in linia de comanda"
--             (fname:_) -> do
--               hFile <- openFile fname ReadMode
--               hClose hFile
--               contents <- hGetContents hFile
--               putStr $ contents

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn "Dati numele fisierului in linia de comanda"
            (fname:_) -> do
              hFile <- openFile fname ReadMode
              contents <- hGetContents hFile
              hClose hFile
              putStr $ contents

impartire :: Integer -> Integer -> Maybe Integer
impartire _ 0 = Nothing
impartire x y = Just (x `div` y)

impartireInLant :: Integer -> Integer -> Integer -> Maybe Integer
impartireInLant x y z = case impartire x y of
                          Nothing -> Nothing
                          Just r -> impartire r z

impartireInLant' :: Integer -> Integer -> Integer -> Maybe Integer
impartireInLant' x y z = (impartire x y) >>= (\r -> impartire r z)

impartireInLant'' :: Integer -> Integer -> Integer -> Maybe Integer
impartireInLant'' x y z = do r <- impartire x y
                             impartire r z



impartire' :: Integer -> Integer -> [Integer]
impartire' _ 0 = []
impartire' x y = [ x `div` y ]

impartireInLant''' :: Integer -> Integer -> Integer -> [Integer]
impartireInLant''' x y z = do r <- impartire' x y
                              impartire' r z
