-- Today ---> Algebraic Data Types (ADTs)
-- Abstract Data Type (ADTs)


-- Algebraic Data Types generalize enums

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show
-- Day is one of the simplest "(disjoint) sum type"s

--   ^^^
-- name of type
--                           ^^^
--                        constructors
nextDay :: Day -> Day
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

data MyBool = MyFalse | MyTrue deriving Show
--            ^^^^^^^
--          constructor

myAnd :: MyBool -> MyBool -> MyBool
myAnd MyTrue MyTrue = MyTrue
myAnd _ _ = MyFalse

convert :: MyBool -> Bool
convert MyFalse = False
convert MyTrue = True

convert' :: Bool -> MyBool
convert' False = MyFalse
convert' True = MyTrue

division :: Int -> Int -> Int
division x y = x `div` y

computation :: Int -> Int -> Int -> Int
computation x y z = x + (division y z)

division' :: Int -> Int -> Int
division' x 0 = 0
division' x y = x `div` y

computation' :: Int -> Int -> Int -> Int
computation' x y z = if (division' y z) == 0 then
                       0
                     else
                       x + (division' y z)

validDivision'' :: Int -> Int -> Bool
validDivision'' x 0 = False
validDivision'' x y = True

division'' :: Int -> Int -> Int
division'' x y = x `div` y

computation'' :: Int -> Int -> Int -> Int
computation'' x y z = if not (validDivision'' y z) then
                       x
                     else
                       x + (division'' y z)

division''' :: Int -> Int -> (Bool, Int)
division''' x 0 = (False, 42)
division''' x y = (True, x `div` y)

computation''' :: Int -> Int -> Int -> Int
computation''' x y z = let (valid, r) = division''' y z in
                         if valid then
                           x + r
                         else
                           x
                           
data Result = Valid Int | Invalid deriving Show
-- Result is a "sum type"

division4 :: Int -> Int -> Result
division4 x 0 = Invalid
division4 x y = Valid (x `div` y) 

computation4 :: Int -> Int -> Int -> Int
computation4 x y z = let result :: Result = division4 y z in
                       case result of
                         Invalid -> x
                         (Valid value) -> x + value

wasItValid :: Result -> Bool
wasItValid Invalid = False
wasItValid (Valid x) = True

wasItValid' :: Result -> Bool
wasItValid' r = case r of
                  Invalid -> False
                  (Valid x) -> True
                  

data Pair = P Int Int deriving Show
-- Pair is simplest "product type"

makePair :: Int -> Int -> Pair
makePair x y = P x y

fstPair :: Pair -> Int
fstPair (P x _) = x

-- won't work:
-- fstPair' :: Pair -> Int
-- fstPair' (makePair x _) = x
-- weird :: Int -> Int
-- weird (x * y) = x

sndPair :: Pair -> Int
sndPair (P _ y) = y


-- algebraic datatype = (disjoint) sum of product types

--data Mystery = A | B Int Mystery deriving Show
data List = Empty | Cons Int List deriving Show

count :: List -> Int
count Empty = 0
count (Cons head tail) = 1 + count tail

countAux :: List -> Int -> Int
countAux Empty acc = acc
countAux (Cons head tail) acc = countAux tail (acc + 1)

count' :: List -> Int
count' list = countAux list 0

sumAll :: List -> Int
sumAll Empty = 0
sumAll (Cons head  tail) = head + sumAll tail
--           vvv   vvvvvvvvvvvvvvvvvvvvvvvvv
--     (Cons 100   (Cons 42 (Cons 23 Empty)))

sumAllAux :: List -> Int -> Int
sumAllAux Empty acc = acc
sumAllAux (Cons head tail) acc = sumAllAux tail (acc + head)

sumAll' :: List -> Int
sumAll' list = sumAllAux list 0

data List' = Empty' | Cons' Bool List' deriving Show

count1 :: List' -> Int
count1 Empty' = 0
count1 (Cons' head tail) = 1 + count1 tail

computePass :: List -> List'
computePass Empty = Empty'
computePass (Cons gpa tail) = if gpa >= 50 then
                                (Cons' True (computePass tail))
                              else
                                (Cons' False (computePass tail))

data MyList = A List | B List' deriving Show


countMyList :: MyList -> Int
countMyList (A list) = count list
countMyList (B list) = count1 list


data ParList a = ParEmpty | ParCons a (ParList a) deriving Show
-- "a" is called a type variable
-- "a" can be instantiated by any particular type
-- For example, "a" could be "int", in which case
-- ParList Int would be the same as List
-- For example, "a" could be "bool", in which case
-- ParList Bool would be the same as List'

parCount :: ParList a -> Int
parCount ParEmpty = 0
parCount (ParCons head tail) = 1 + parCount tail

countList :: [a] -> Int
countList [] = 0
countList (hd : tl) = 1 + countList tl

qs :: [Int] -> [Int]
qs [] = []
qs (hd:tl) = qs (filter (<=hd) tl) ++ [hd] ++ qs (filter (>hd) tl)

data ResultChar = ValidChar Char | InvalidChar deriving Show

firstChar :: String -> ResultChar
firstChar (hd : tl) = ValidChar hd
firstChar [] = InvalidChar

data Option a = None | Some a deriving Show

firstChar' :: String -> Option Char
firstChar' (hd : tl) = Some hd
firstChar' [] = None

division5 :: Int -> Int -> Option Int
division5 x 0 = None
division5 x y = Some (x `div` y) 

firstChar'' :: String -> Maybe Char
firstChar'' (hd : tl) = Just hd
firstChar'' [] = Nothing

division6 :: Int -> Int -> Maybe Int
division6 x 0 = Nothing
division6 x y = Just (x `div` y) 

data MyExc = DivisionByZero | SquareRootOfNegative deriving Show

computation1 :: Integer -> Integer -> Either Integer MyExc
computation1 x 0 = Right DivisionByZero
computation1 x y = let result = (fromInteger (x `div` y)) in
                     if result < 0 then
                       Right SquareRootOfNegative
                     else
                       Left (floor (sqrt result))
                       
