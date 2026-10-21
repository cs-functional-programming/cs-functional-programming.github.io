data MyBool = MyFalse | MyTrue deriving Show

negation :: MyBool -> MyBool
negation MyFalse = MyTrue
negation MyTrue = MyFalse

data MyPair = Pair MyBool MyBool deriving Show
--                 ^^^^^^^^^^^^^
--                  product type
-- 

-- Pair is a constructor for the datatype MyPair
-- Pair is like a box where two boolean values can fit

fstComponent :: MyPair -> MyBool
fstComponent (Pair x y) = x

-- How many values of type MyPaire are there?
-- Four values:
-- (1) Pair MyFalse MyFalse, (2) Pair MyFalse MyTrue,
-- (3) Pair MyTrue MyFalse, (4) Pair MyTrue MyTrue

data Mistery = Asdf | Qwerty MyPair deriving Show
--                 ^^^
--              disjoint sum/union type

-- How many values of type Mistery are there?
-- Five:
-- (1) Asdf,
-- (2) Qwerty (Pair MyFalse MyFalse), (3) Qwerty (Pair MyFalse MyTrue),
-- (4) Qwerty (Pair MyTrue MyFalse), (5) Qwerty (Pair MyTrue MyTrue)

-- Algebraic datatypes = sum + product

-- Good idea: for computation that can go wrong,
-- return a result of type "Maybe *", with "Nothing"
-- for the errorneous case.
division :: Integer -> Integer -> Maybe Integer
division x 0 = Nothing
division x y = Just (x `div` y)

firstCharacter :: String -> Maybe Char
firstCharacter [] = Nothing
firstCharacter (c:_) = Just c

squareRoot1 :: Integer -> Integer
squareRoot1 x = floor (sqrt (fromInteger x))

squareRoot2 :: Integer -> Maybe Integer
squareRoot2 x | x < 0     = Nothing
              | otherwise = Just (floor (sqrt (fromInteger x)))

-- Closer look at division and squareRoot2.
-- Issue
value1 :: Maybe Integer
value1 = squareRoot2 (-10)

value2 :: Maybe Integer
value2 = division 10 0

-- What are value1 and value2? They are both "Nothing".
-- We can distinguish between "why" the two are "Nothing".

-- If you need not only the fact that the computation went wrong,
-- but also a reason why the computation went wrong,
-- you want to use the datatype "Either".

data ReasonComputationFails = DivisionByZero | SquareRootOfNegative deriving Show

division2 :: Integer -> Integer -> Either ReasonComputationFails Integer
division2 x 0 = Left DivisionByZero
division2 x y = Right (x `div` y)

squareRoot3 :: Integer -> Either ReasonComputationFails Integer
squareRoot3 x | x < 0     = Left SquareRootOfNegative
              | otherwise = Right (floor (sqrt (fromInteger x)))

value3 :: Either ReasonComputationFails Integer
value3 = squareRoot3 (-10)

value4 :: Either ReasonComputationFails Integer
value4 = division2 10 0

-- Case study

-- A good place where you *want* to use ADTs.
-- Process symbolic expressions.


-- arithmetic expression
-- Brackets ---> let's get back to this
-- GreaterThan --> cannot be an arithmetic expression (would work for Boolean Expressions)
data AExp = Const Int | Plus AExp AExp | Minus AExp AExp | Times AExp AExp
          | UnaryMinus AExp | Variable String deriving Show

data Dictionary = Empty | Cons String Int Dictionary deriving Show

myDictionary :: Dictionary
myDictionary = Cons "x" 5 (Cons "y" 7 Empty)

find :: String -> Dictionary -> Maybe Int
find x Empty = Nothing
find x (Cons variableName value rest) = if x == variableName then
                                          Just value
                                        else
                                          find x rest

-- what interesting Haskell computations could we define on AExps?
evaluate0 :: AExp -> Int
evaluate0 (Const c) = c
evaluate0 (Plus e1 e2) = evaluate0 e1 + evaluate0 e2
evaluate0 (Minus e1 e2) = evaluate0 e1 - evaluate0 e2
evaluate0 (Times e1 e2) = evaluate0 e1 * evaluate0 e2
evaluate0 (UnaryMinus e) = -(evaluate0 e)
evaluate0 (Variable variableName) = 0

evaluate :: AExp -> Dictionary -> Maybe Int
evaluate (Const c) dict = Just c
evaluate (Plus e1 e2) dict = case evaluate e1 dict of
                          Nothing -> Nothing
                          Just v1 -> case evaluate e2 dict of
                                       Nothing -> Nothing
                                       Just v2 -> Just (v1 + v2)
evaluate (Minus e1 e2) dict = case evaluate e1 dict of
                          Nothing -> Nothing
                          Just v1 -> case evaluate e2 dict of
                                       Nothing -> Nothing
                                       Just v2 -> Just (v1 - v2)
evaluate (Times e1 e2) dict = case evaluate e1 dict of
                          Nothing -> Nothing
                          Just v1 -> case evaluate e2 dict of
                                       Nothing -> Nothing
                                       Just v2 -> Just (v1 * v2)
evaluate (UnaryMinus e) dict = case evaluate e dict of
                            Nothing -> Nothing
                            Just v -> Just (-v)
evaluate (Variable variableName) dict = find variableName dict

data BinaryOperation = Plus' | Minus' | Times' deriving Show

data AExp' = Const' Int | BinOp' BinaryOperation AExp' AExp'
          | UnaryMinus' AExp' | Variable' String deriving Show

evaluate' :: AExp' -> Dictionary -> Maybe Int
evaluate' (Const' c) dict = Just c
evaluate' (BinOp' op e1 e2) dict = case evaluate' e1 dict of
                          Nothing -> Nothing
                          Just v1 -> case evaluate' e2 dict of
                                       Nothing -> Nothing
                                       Just v2 -> Just (case op of
                                                          Plus' -> v1 + v2
                                                          Minus' -> v1 - v2
                                                          Times' -> v1 * v2
                                                       )
evaluate' (UnaryMinus' e) dict = case evaluate' e dict of
                            Nothing -> Nothing
                            Just v -> Just (-v)
evaluate' (Variable' variableName) dict = find variableName dict



{-
Two approaches to defining arithmetic expressions:
- as an ADT;
- as a class hierarchy.
We will discuss advantages / disadvantages.
-}

{-
Two extensions:
(1) add a type of expression that is missing currently

Let us add division.
FOR ADTS:
- two things
- (1) add in the new case
- (2) add an equation for every computation
FOR CLASS HIERARCHY
- just one thing
- (1) derive a new class
- DO NOT TOUCH EXISTING COMPUTATIONS AT ALL (the change is more modular)

(2) add a computation that is missing currently
FOR ADTS
- just one thing: (1) add in a new function for the computation
FOR CLASSES
- several things: add a new method in *each class* of the hierarchy

EXPRESSION PROBLEM -> it is easier to define a new case for class hierarchies
                   -> it is easier to define new computations for ADTs

-}


data Exp = Constant Int | BPlus Exp Exp | BMinus Exp Exp | BTimes Exp Exp
          | UMinus Exp | Var String | BDiv Exp Exp deriving Show
--                                     ^^^^^^^^^^^^
--                           (1) add a constructor for expressions representing divisions

evaluate1 :: Exp -> Int
evaluate1 (Constant c) = c
evaluate1 (BPlus e1 e2) = evaluate1 e1 + evaluate1 e2
evaluate1 (BMinus e1 e2) = evaluate1 e1 - evaluate1 e2
evaluate1 (BTimes e1 e2) = evaluate1 e1 * evaluate1 e2
evaluate1 (BDiv e1 e2) = evaluate1 e1 `div` evaluate1 e2 --- (2) add an equation
evaluate1 (UMinus e) = -(evaluate1 e)
evaluate1 (Var variableName) = 0

simplify :: Exp -> Exp
simplify (Constant c) = Constant c
simplify (BPlus e1 e2) = case (simplify e1, simplify e2) of
                           (Constant c1, Constant c2) -> Constant (c1 + c2)
                           (Constant 0, e2') -> e2'
                           (e1', Constant 0) -> e1'
                           (e1', e2') -> BPlus e1' e2'
simplify (BTimes e1 e2) = case (simplify e1, simplify e2) of
                           (Constant c1, Constant c2) -> Constant (c1 * c2)
                           (Constant 0, e2') -> Constant 0
                           (e1', Constant 0) -> Constant 0
                           (Constant 1, e2') -> e2'
                           (e1', Constant 1) -> e1'
                           (e1', e2') -> BTimes e1' e2'
simplify (BMinus e1 e2) = BMinus (simplify e1) (simplify e2)
simplify (BDiv e1 e2) = BDiv (simplify e1) (simplify e2)
simplify (UMinus e) = UMinus (simplify e)
simplify (Var v) = Var v

