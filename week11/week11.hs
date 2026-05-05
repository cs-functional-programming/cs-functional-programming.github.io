import Control.Monad.State
import Data.Char

type Stack = [Int]

empty :: Stack
empty = []

pop :: Stack -> (Int, Stack)
pop (hd:tl) = (hd, tl)

push :: Int -> Stack -> Stack
push x st = x:st


-- vreau sa scriem urmatorul calcul:
-- pornind de la o stiva,
-- extrag doua elemente,
-- le adun,
-- pun la loc pe stiva suma lor

calcul :: Stack -> Stack
calcul st = let (n1, st1) = pop st in
            let (n2, st2) = pop st1 in
              push (n1 + n2) st2
{-
Nu as putea sa scriu calculul de mai sus
intr-un mod mai lizibil?
calcul = do n1 <- pop
            n2 <- pop
            push (n1 + n2)
-}

-- Paranteza
data Viteza = Viteza Float deriving Show
{-
struct Viteza
{
  float temp;
};

n1' :: Viteza
n1' = Viteza 100
Viteza *v = new Viteza;
v->temp = 100;
-}
data Distanta = Distanta Float deriving Show
data Timp = Timp Float deriving Show

calculDistanta :: Viteza -> Timp -> Distanta
calculDistanta (Viteza v) (Timp t) = Distanta (v * t)

calculDistanta' :: Float -> Float -> Float
calculDistanta' v t = v * t

n1 :: Float
n1 = 100 -- viteza

n2 :: Float
n2 = 2 -- timp

n3 :: Float
n3 = 50 -- distanta

n1' :: Viteza
n1' = Viteza 100

n2' :: Timp
n2' = Timp 2

n3' :: Distanta
n3' = Distanta 50

newtype Viteza' = Viteza' Float deriving Show
{-
typedef Viteza' float;
n1' :: Viteza'
n1' = Viteza' 100

Viteza' v;
v = 100;
-}
newtype Distanta' = Distanta' Float deriving Show
newtype Timp' = Timp' Float deriving Show

-- inchis paranteza

-- Ce face push?
-- Intorce... nu intoarce nimic util.
-- Schimba starea stivei adaugand un element in fata
-- stivei.

-- Ce face pop?
-- Intoarce elementul din varful stivei.
-- Schimba starea stergand elementul din varful stivei.

--       starea calculului (in exemplul meu: o stiva)
--           vvv
newtype State' s a = State' { runState' :: s -> (a, s) }
--             ^^^
--          ce intoarce calculul
--        pentru pop: un int
--        pentru push: nimic util

pop' :: State' Stack Int
pop' = State' $ \(hd:tl) -> (hd, tl)

push' :: Int -> State' Stack ()
push' x = State' $ \st -> ((), (x:st))

secv :: State' s a -> State' s b -> State' s b
secv c1 c2 = State' $ \s -> let (_, s') = runState' c1 s in
                           runState' c2 s'

secv' :: State' s a -> (a -> State' s b) -> State' s b
secv' c1 c2 = State' $ \s -> let (r, s') = runState' c1 s in
                            runState' (c2 r) s'

-- fmap :: (a -> b) -> State' s a -> State' s b

instance Functor (State' s) where
  fmap f c = State' $ \s -> let (r, s') = runState' c s in
                             (f r, s')
  
instance Applicative (State' s) where
  pure x = State' $ \s -> (x, s)
  (<*>) (fab :: State' s (a -> b)) (fa :: (State' s a)) =
    State' $ \s -> let (f, s') = runState' fab s in
                    let (a, s'') = runState' fa s' in
                      (f a, s'')
  
instance Monad (State' s) where
  (>>=) c1 c2 = secv' c1 c2

calcul'' :: State' Stack Int
calcul'' = secv pop' pop'

calcul''' :: State' Stack Int
calcul''' = secv' pop' (\r -> (secv' pop' (\r' -> (State' $ \st -> (r + r', st)))))

calcul4 :: State' Stack Int
calcul4 = pop' >>= (\r ->
                      (pop' >>= (\r' ->
                                   (State' $ \st -> (r + r', st)))))

calcul5 :: State' Stack Int
calcul5 = do r <- pop'
             r' <- pop'
             return (r + r')

calcul6 :: State' Stack Int
calcul6 = do r <- pop'
             r' <- pop'
             push' (r + r')
             return (r + r')

calcul7 :: State' Stack ()
calcul7 = do r <- pop'
             r' <- pop'
             push' (r + r')
             return ()

calcul8 :: State' Stack ()
calcul8 = do r <- pop'
             r' <- pop'
             push' (r + r')

calcul' :: State' Stack ()
calcul' = State' $ \st -> let (n1, st1) = runState' pop' st in
                         let (n2, st2) = runState' pop' st1 in
                         let sum = n1 + n2 in
                           runState' (push' sum) st2

            -- let (n1, st1) = pop st in
            -- let (n2, st2) = pop st1 in
            --   push (n1 + n2) st2

type Pereche = (Int, Int)

get0 :: State' Pereche Int
get0 = State' $ \(x, y) -> (x, (x, y))

get1 :: State' Pereche Int
get1 = State' $ \(x, y) -> (y, (x, y))

put0 :: Int -> State' Pereche ()
put0 n = State' $ \(x, y) -> ((), (n, y))

put1 :: Int -> State' Pereche ()
put1 n = State' $ \(x, y) -> ((), (x, n))

calcul_perechi :: State' Pereche Int
calcul_perechi = do put0 10
                    put1 23
                    put1 13
                    put1 3
                    put0 7
                    get0

calcul_perechi' :: State' Pereche Int
calcul_perechi' = do n0 <- get0
                     n1 <- get1
                     if n0 < n1 then
                       do put1 (n1 - n0)
                          calcul_perechi'
                     else if n1 < n0 then
                       do put0 (n0 - n1)
                          calcul_perechi'
                     else
                       return n0




get0' :: State Pereche Int
get0' = state $ \(x, y) -> (x, (x, y))

get1' :: State Pereche Int
get1' = state $ \(x, y) -> (y, (x, y))

put0' :: Int -> State Pereche ()
put0' n = state $ \(x, y) -> ((), (n, y))

put1' :: Int -> State Pereche ()
put1' n = state $ \(x, y) -> ((), (x, n))

calcul_perechiState :: State Pereche Int
calcul_perechiState = do put0' 10
                         put1' 23
                         put1' 13
                         put1' 3
                         put0' 7
                         (x, y) <- get
                         return (x + y)

-- parser

-- Un parser = un program care modifica string-ul care trebuie parsat
-- Starea parserului = string-ul care a ramas + pozitia in string

-- runState myParse "42+26"    --> 68

type ParserState = (String, Int)

type Parser a = State ParserState (Either String a)

nextChar :: Parser Char
nextChar = do (s, p) <- get
              case s of
                [] -> return $ Left ("Unexpected end of string at " ++ show p)
                (hd:tl) -> do
                  put (tl, p + 1)
                  return (Right hd)

charp :: Char -> Parser Char
charp expected = do (_, p) <- get
                    result <- nextChar
                    case result of
                      Left error -> return (Left error)
                      Right c -> if c == expected then
                                   return (Right c)
                                 else
                                   return $ Left ("Expected " ++ [expected] ++ " at position " ++ (show p))


digit :: Parser Char
digit = do (_, p) <- get
           result <- nextChar
           case result of
             Left err -> return $ Left err
             Right c -> if isDigit c then
                          return $ Right c
                        else
                          return $ Left ("Expected digit at position " ++ show p)

many :: Parser a -> Parser [a]
many parser = do
  (input, pos) <- get
  result <- parser
  case result of
    Left err -> do
      put (input, pos)
      return $ Right []
    Right x -> do
      result_tail <- many parser
      case result_tail of
        Left err -> return $ Left err
        Right tl -> return $ Right (x : tl)

number :: Parser Int
number = do result <- many digit
            case result of
              Left err -> return $ Left err
              Right lista_cifre -> return $ Right (read lista_cifre)

-- parsam expresii de forma "numar1+numar2"
exprParser :: Parser (Int, Int)
exprParser = do r1 <- number
                charp '+'
                r2 <- number
                case (r1, r2) of
                  (Left err1, _) -> return $ Left err1
                  (_, Left err2) -> return $ Left err2
                  (Right n1, Right n2) -> return $ Right (n1, n2)

evalParser :: Parser Int
evalParser = do r1 <- number
                charp '+'
                r2 <- number
                case (r1, r2) of
                  (Left err1, _) -> return $ Left err1
                  (_, Left err2) -> return $ Left err2
                  (Right n1, Right n2) -> return $ Right (n1 + n2)

-- monadic parser combinators
-- biblioteca MegaParsec       "12+(23+x)"
