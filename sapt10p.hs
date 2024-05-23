import Control.Monad.State.Lazy

type Stack = [Int]

-- push :: Int -> Stack -> ((), Stack)
-- push x s = ((), x : s)

-- pop :: Stack -> (Int, Stack)
-- pop (hd:tl) = (hd, tl)

-- manip :: Stack -> (Int, Stack)
-- manip s1 =
--   let ((), s2) = push 3 s1 in
--     let (x, s3) = pop s2 in
--       let (y, s4) = pop s3 in
--         (y, s4)

pop :: State Stack Int
pop = state $ \(hd:tl) -> (hd, tl)

push :: Int -> State Stack ()
push x = state $ \s -> ((), x : s)

manip :: State Stack Int
manip = do
  push 3
  x <- pop
  pop

setx :: Int -> State (Int, Int) ()
setx x = state $ \(_, y) -> ((), (x, y))

sety :: Int -> State (Int, Int) ()
sety y = state $ \(x, _) -> ((), (x, y))

getx :: State (Int, Int) Int
getx = state $ \(x, y) -> (x, (x, y))

gety :: State (Int, Int) Int
gety = state $ \(x, y) -> (y, (x, y))

mygcd :: State (Int, Int) Int
mygcd = do
  x <- getx
  y <- gety
  if y == 0 then
    return x
  else do
    setx y
    sety (mod x y)
    mygcd

dist :: Double -> Double -> Double
dist v t = v * t

viteza :: Double -> Double -> Double
viteza d t = d / t

-- data Viteza = Viteza Double deriving (Show, Eq)
-- data Distanta = Distanta Double deriving (Show, Eq)
-- data Timp = Timp Double deriving (Show, Eq)

newtype Viteza = Viteza Double deriving (Show, Eq)
newtype Distanta = Distanta Double deriving (Show, Eq)
newtype Timp = Timp Double deriving (Show, Eq)

dist' :: Viteza -> Timp -> Distanta
dist' (Viteza v) (Timp t) = Distanta (v * t)

viteza' :: Distanta -> Timp -> Viteza
viteza' (Distanta d) (Timp t) = Viteza (d / t)

