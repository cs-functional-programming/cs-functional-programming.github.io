import System.Environment
import System.IO

-- UNIT TYPE: ()
-- It only has one value: ()

f :: Int -> ()
f x = ()

g :: () -> Int
g () = 42

-- What is the point????!!!????

h :: Int
h = 42

-- Why would I need unit????

-- create a function that
-- does not return anything

-- what is the point of such a
-- function???

-- In C, it would be interesting
-- such a function could have side
-- effects.

-- () corresponds to void in C(++)

-- The way to have side effects
-- in Haskell is to use the
-- following trick:

-- We will a type "IO a" whose
-- values are commands/actions.

-- If a Haskell program has a
-- function called "main" that
-- returns a command, then
-- the system will execute the
-- command/action after it is
-- evaluated.

--main :: IO ()
--main = putStrLn "Hello, World!"

-- 1. Firstly, the program
--    will evaluate the expression
--    "main" and compute a command
-- 2. Secondly, the command
--    will be executed


-- IO actions/IO commands
-- can be executed and, at the end,
-- if they execute successfully
-- they can produce a value (the
-- value comes after execution is done)

-- When the value is not useful,
-- we use the unit value.

-- "IO a" are actions that
-- eventually produce a value
-- of type "a".

main0 :: IO String
main0 = getLine

-- If I evaluate in ghci a value
-- of type IO a, not only will
-- ghci compute the action/command
-- but also execute it.

main1 :: IO ()
main1 = putStrLn "Hello, World!"

-- One way to create IO actions
-- (or IO commands) is to use
-- builtin functions that return
-- such actions: putStrLn, getLine.

-- But it would be useful to
-- combine such actions together.

matei2 :: IO ()
matei2 = (putStrLn "Hello, World!") >>
         (putStrLn "Welcome to Haskell!")

main3 :: IO ()
main3 = putStrLn "Hello, World!" >>
        putStrLn "Welcome to Haskell!" >>
        putStrLn "Third action here!"

main4 :: IO ()
main4 = putStrLn "What is your name? " >>
        getLine >>
        putStrLn "Welcome!"

-- main5 :: IO ()
-- main5 = putStrLn "What is your name? " >>
--         --getLine >>
--                                            String
--                                            vvvvv
--         putStrLn ("Welcome, " ++ getLine ++ "!")
--                                  ^^^^^^^    ^^^
--                                :: IO String

-- Keep in mind Matei's question:
-- could we cast the IO String to a String (???)

-- main6 :: IO ()
-- main6 = putStrLn "What is your name? " >>
--         putStrLn ("Welcome, " ++ (getLine :: String) ++ "!")

-- (>>) :: IO a -> IO b -> IO b
-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- "bind" operator

main7 :: IO ()
main7 = putStrLn "What is your name? " >>
        getLine >>=
        (\name -> putStrLn ("Welcome, " ++ name ++ "!"))

-- In ghci, if you evaluate a typical value
--    1. evalutes the result
--    2. uses the class Show to print up the result
-- In ghci, if you evaluate a value of type "IO a"
--    1. evaluates the result (which is an action)
--    2. instead of printing the action, it executes it

-- if the type of the expression is "IO a " then
--   execute the result
-- else if the type of the expression in in the class "Show"
--   print the result
-- else
--   give back an error.

-- Sorin: Q: can we produce a value (IO 2).

-- A function computes a value.
-- An IO action will produce a value (at the end of the action).


-- I find this syntax cumbersome:
-- main7 :: IO ()
-- main7 = putStrLn "What is your name? " >>
--         getLine >>=
--         (\name -> putStrLn ("Welcome, " ++ name ++ "!"))

-- I have a better syntax, which is called "do-notation"
-- and which the compiler translates into the syntax above
main8 :: IO ()
main8 = do putStrLn "What is your name? "
           name <- getLine
           putStrLn ("Welcome, " ++ name ++ "!")

-- the lexer will translate main8 into main9, and somewhere
-- after the parser there will be a phase that translates
-- main8 into main7.

-- indentation-sensitive
main9 :: IO ()
main9 = do { putStrLn "What is your name? " ;
            name <- getLine ;
          putStrLn ("Welcome, " ++ name ++ "!") }

main10 :: IO ()
main10 = do putStrLn "What is your name? "
            name <- getLine
            putStrLn ("Welcome, " ++ name ++ "!")
            main10

-- But what if I want to stop the recursion at some point?
-- We need a base case

emptyAction :: IO ()
emptyAction = return ()

main11 :: IO ()
main11 = do putStrLn "What is your name? "
            name <- getLine
            if name == "" then
              return () -- the action that does nothing and produces the value "()"
            else do
              putStrLn ("Welcome, " ++ name ++ "!")
              main11

main12 :: IO ()
main12 = do putStrLn "What is your name? "
            return () -- In Haskell, return is not a control-flow structure
                      -- But it returns an action
            name <- getLine
            if name == "" then
              return ()
            else
              do putStrLn ("Welcome, " ++ name ++ "!")
                 main12

produce42 :: IO Integer
produce42 = return 42


-- myGetLine :: String
-- myGetLine = do s <- getLine
--                return s

-- main13 = do putStrLn "What is your name? "
--             if name == "" then
--               return ()
--             else
--               do putStrLn ("Welcome, " ++ myGetLine ++ "!")
--                  main13

-- It is not possible to "convert" from IO String
-- to String.

-- It is impossible to write a function
-- transform :: IO String -> String
-- The type system does not allow this
-- You cannot "extract" a String from
-- under an IO action.

-- In fact, you can think of "IO String"
-- as a String that is contaminated, you cannot
-- make it escape the IO monad.

-- You cannot call an action from a pure function.

appendIOString :: IO String -> IO String -> IO String
appendIOString action1 action2 = do s1 <- action1
                                    s2 <- action2
                                    return (s1 ++ s2)

main14 :: IO ()
main14 = do putStrLn "Enter two lines"
            result <- appendIOString getLine getLine
            putStrLn ("result = " ++ result)


-- functions in the IO monad are impure
-- functions outside of the IO monad are pure

-- Typically: you want as many pure functions
-- as possible.

-- As few impure functions as possible.

-- Let us write a program that prints a Haskell file
-- without the comments.
-- Comments = lines starting with '--'

printWithoutComments :: String -> IO ()
printWithoutComments [] = return ()
printWithoutComments ('-':'-':rest) = printFromNewline rest
printWithoutComments s = printToNewline s

printFromNewline :: String -> IO ()
printFromNewline [] = return ()
printFromNewline ('\n':rest) = printWithoutComments rest
printFromNewline (c:rest) = printFromNewline rest

printToNewline :: String -> IO ()
printToNewline [] = return ()
printToNewline ('\n':rest) = do putStrLn ""
                                printWithoutComments rest
printToNewline (c:rest) = do putStr [c]
                             printToNewline rest

-- Approach #1 (the wrong approach!!!)
mainwrong = do args <- getArgs
               case args of
                 [] -> putStrLn "./week09 <filename>"
                 (fn:_) -> do handle <- openFile fn ReadMode
                              contents <- hGetContents handle
                              printWithoutComments contents

-- CORE PART - purely functional
isCommentLine :: String -> Bool
isCommentLine ('-':'-':_) = True
isCommentLine _ = False

process :: String -> String
process contents = unlines (filter (not . isCommentLine) (lines contents))

-- THIN OUTER LAYER - the IO action
main = do args <- getArgs
          case args of
            [] -> putStrLn "./week09 <filename>"
            (fn:_) -> do handle <- openFile fn ReadMode
                         contents <- hGetContents handle
                         putStrLn (process contents)
