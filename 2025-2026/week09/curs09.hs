import System.Environment
import System.IO
import Control.Exception

-- Monada IO
-- modul in care pot scrie programe de facto imperative
-- in Haskell.

-- Tipul unit, "()", are o singura valoare posibila: "()"
-- Are acelasi rol cu tipul "void" din C/C++/Java/C#

f :: Int -> ()
f x = ()

g :: () -> Int
g () = 42

h :: Int
h = 42

-- Tipul "IO a" are ca valori
-- comenzi/actiuni IO care, daca sunt executate, produc
-- la sfarsit o valoare de tip "a".

-- putStrLn :: String -> IO ()
-- putStrLn primeste la intrare un sir de caractere
--          intoarce o actiune IO care, cand este executata,
--          afiseaza pe ecran sirul de caractere, trece la linia
--          urmatoare, si produce o valoare de tip ()

-- Terminologie:
--    actiunea produce o valoare
--    o functie returneaza/intoarce o valoare

-- truc: daca exista in program o actiune IO care se numeste "main",
-- programul se executa in felul urmator:
--     se evalueaza "main"
--     se executa actiunea rezultata

main01 :: IO ()
main01 = putStrLn "Hello, World!"

main02 :: IO ()
main02 = (putStrLn "(main02) Hello, ") >> (putStrLn "World!")

main03 :: IO ()
main03 = (putStr "(main03) Hello, ") >> (putStrLn "World!")

main04 :: IO ()
main04 = (putStr "(main03) Hello, ") >>
         (putStr "asdf") >>
         (putStrLn "World!")

main05 :: IO ()
main05 = (putStrLn "Hello!") >>
         (putStr "What is your name?") >>
         getLine >>
         (putStrLn "Bye, bye!")

main06 :: IO ()
main06 = (putStrLn "Hello!") >>
         (putStr "What is your name?") >>
         (getLine >>=
          (\name -> putStrLn $ "Hello, " ++ name ++ "!"))

-- Pentru monada IO, sistemul suporta zahar sintactic (notatia "do")
main07 :: IO ()
main07 = do putStrLn "Hello!"
            putStr "What is your name? "
            name <- getLine
            putStrLn $ "Bye, " ++ name ++ "!"

-- Greseala Frecventa!
-- main08 :: IO ()
-- main08 = do putStrLn "Hello!"
--             putStr "What is your name? "
--             let name = getLine
--             putStrLn $ "Bye, " ++ name ++ "!"


-- Atentie! Notatia do este "indentation-sensitive"
-- main09 :: IO ()
-- main09 = do putStrLn "Hello!"
--            putStr "What is your name? "
--              name <- getLine
--             putStrLn $ "Bye, " ++ name ++ "!"

-- Varianta notatie do:

main10 :: IO ()
main10 = do {
          putStrLn "Hello!";
            putStr "What is your name? ";
        name <- getLine;
            putStrLn $ "Bye, " ++ name ++ "!" }

-- bucla infinita
-- main11 :: IO ()
-- main11 = do putStr "What is your name?"
--             name <- getLine
--             putStrLn $ "Hello, " ++ name ++ "!"
--             main11

main12 :: IO ()
main12 = do putStr "What is your name?"
            name <- getLine
            if name == "" then do
              putStr ""
            else do
              putStrLn $ "Hello, " ++ name ++ "!"
              main12

main13 :: IO ()
main13 = do putStr "What is your name?"
            name <- getLine
            if name == "" then
              return () -- actiune care nu face nimic si produce "()"
            else do
              putStrLn $ "Hello, " ++ name ++ "!"
              main13

-- Atentie! "return" este o functie care oricare alta, nu afecteaza
-- fluxul de control al programului (ca in C/C++)

main14 :: IO ()
main14 = do putStrLn "Hello, World!"
            return ()
            putStrLn "Ajung aici?"

main15 :: IO ()
main15 = do args <- getArgs
            progName <- getProgName
            putStrLn $ "Args: " ++ (show args)
            putStrLn $ "Prog: " ++ progName

-- Este imposibil de scris o functie

-- transform :: IO a -> a

-- Intentionat!

-- getLine' :: String
-- getLine' = do s <- getLine
--               return s

-- Nu pot scapa de markerul "IO"
-- Daca o functie este "contaminata" de IO, nu o pot decontamina
-- Functiile care intorc actiuni pot fi folosite doar
-- in alte functii care intorc si ele actiuni.
-- Nu pot sa folosesc o functie IO intr-o functie pura.

-- Contraexemplu perfect despre cum se scrie codul Haskell.
-- Exemplu despre cum nu trebuie scris codul Haskell.
printWithoutComments :: String -> IO ()
printWithoutComments [] = return ()
printWithoutComments ('-':'-':rest) = printFromFirstNewLine rest
printWithoutComments s = printToFirstNewLine s

printFromFirstNewLine :: String -> IO ()
printFromFirstNewLine [] = return ()
printFromFirstNewLine ('\n':rest) = printWithoutComments rest
printFromFirstNewLine (_:rest) = printFromFirstNewLine rest

printToFirstNewLine :: String -> IO ()
printToFirstNewLine [] = return ()
printToFirstNewLine ('\n':rest) = do putStrLn ""
                                     printWithoutComments rest
printToFirstNewLine (c:rest) = do putStr [c]
                                  printToFirstNewLine rest

main16 :: IO ()
main16 = do args <- getArgs
            case args of
              [] -> putStrLn "curs09 <filename.hs>"
              (filename:_) -> do handle <- openFile filename ReadMode
                                 contents <- hGetContents handle
                                 printWithoutComments contents

-- In Haskell vreau sa structurez programele in felul urmator:
-- * cat mai putin cod in monada IO
-- * cat mai mult cod pur

isCommentLine :: String -> Bool
isCommentLine ('-':'-':_) = True
isCommentLine _ = False

removeComments :: String -> String
removeComments s = unlines $ filter (not . isCommentLine) (lines s)

main17 :: IO ()
main17 = do args <- getArgs
            case args of
              [] -> putStrLn "curs09 <filename.hs>"
              (filename:_) -> do handle <- openFile filename ReadMode
                                 contents <- hGetContents handle
                                 putStrLn $ removeComments contents

main18 :: IO ()
main18 = do args <- getArgs
            case args of
              [] -> putStrLn "curs09 <filename.hs>"
              (filename:_) -> catch
                                (do putStrLn $ "Init"
                                    handle <- openFile filename ReadMode
                                    putStrLn $ "Opened file"
                                    contents <- hGetContents handle
                                    putStrLn $ "Get contents"
                                    putStrLn $ removeComments contents)
                                (\(e :: IOError) -> putStrLn "Hopa!")

main19 :: IO ()
main19 = do args <- getArgs
            case args of
              [] -> putStrLn "curs09 <filename.hs>"
              (filename:_) -> do putStrLn $ "Init"
                                 handle <- openFile filename ReadMode
                                 putStrLn $ "Opened file"
                                 contents <- hGetContents handle
                                 putStrLn $ "Get contents"
                                 putStrLn $ removeComments contents
                              `catch`
                              (\(e :: IOError) -> putStrLn $ "Hopa!" ++ show e)

main20 :: IO ()
main20 = do value <- return (10 `div` 0)
            putStrLn $ show value
            return ()
         `catch`
         (\(e :: ArithException) -> putStrLn $ "Hopa!" ++ show e)

f' :: Int -> ()
f' x | x > 20 = ()

main21 :: IO ()
main21 = do value <- return (f' 10)
            putStrLn $ show value
            return ()
         `catch`
         (\(e :: PatternMatchFail) -> putStrLn $ "Hopa!" ++ show e)


main22 :: IO ()
main22 = do args <- getArgs
            case args of
              [] -> putStrLn "curs09 <filename.hs>"
              (filename:_) -> do handle <- openFile filename ReadMode
                                 contents <- hGetContents handle
                                 hClose handle -- Atentie!
                                 putStrLn $ contents
                              `catch`
                              (\(e :: IOError) -> putStrLn $ "Hopa!" ++ show e)

main :: IO ()
main = main22

-- Truc: daca in ghci o expresie se evalueaza la o actiune,
-- sistemul nu numai ca evalueaza expresia, dar si executa
-- actiunea dupa evaluare




-- Ce este o monada?
-- (>>=)

test00 :: Maybe Int
test00 = return 10

test01 :: Maybe Int
test01 = do return 10
            return 20
            
test02 :: Maybe Int -> Maybe Int
test02 x = do i <- x
              return (i + 10)

impartire :: Maybe Int -> Maybe Int -> Maybe Int
impartire x y = do i <- x
                   j <- y
                   if j == 0 then
                     return i -- de gandit ce as putea face aici
                   else
                     return (i `div` j)

list01 :: [Int]
list01 = do return 10
            return 20

list02 :: [Int] -> [Int]
list02 l = do e <- l
              return (e + 10)
