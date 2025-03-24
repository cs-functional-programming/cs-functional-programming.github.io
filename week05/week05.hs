





add :: Int -> Int -> Int
-- Unofficially: add takes two Int's, gives me back one Int
-- Officially: add is a higher order function:
--             it takes one Int as an argument (call it x)
--             returns me a function that, when called with an argument y,
--             increments y by x
add x y = x + y

app :: String -> String -> String
app s1 s2 = s1 ++ s2

swap :: (String -> String -> String) -> (String -> String -> String)
swap f = \ s1 s2 -> f s2 s1

mistery :: (Int -> Int) -> Int
mistery f = f 0

adder :: Int -> (Int -> Int)
adder step = \ x -> x + step

adder' :: Int -> Int -> Int
adder' step = \ x -> x + step

adder'' :: Int -> Int -> Int
adder'' step x = x + step

g = adder 3
g' = adder 10
g'' = adder'' 3
g''' = adder'' 10

-- "curried" functions
add3 :: Int -> (Int -> (Int -> Int))
add3 x y z = x + y + z

-- "uncurried" functions
add2 :: (Int, Int) -> Int
add2 (x, y) = x + y

curry' :: ((Int, Int) -> Int) -> (Int -> Int -> Int)
curry' f = \ x y -> f (x, y)

uncurry' :: (Int -> Int -> Int) -> ((Int, Int) -> Int)
uncurry' f = \ (x,y) -> f x y

mistery' :: (a -> b) -> (c -> a) -> c -> b
mistery' f g x = f (g x)

-- mistery' is function composition.
-- (.) in standard library

inc_grade :: [Float] -> [Float]
inc_grade [] = []
inc_grade (hd : tl) = (hd + 1) : (inc_grade tl)

fac_grade :: [Float] -> [Float]
fac_grade [] = []
fac_grade (hd : tl) = (hd * 1.1) : (fac_grade tl)

process_grade :: Float -> Float
--process_grade x = x + 1
process_grade x = x * 1.1

process_list :: (Float -> Float) -> [Float] -> [Float]
process_list _ [] = []
process_list f (hd : tl) = (f hd) : (process_list f tl)

process_strings :: [String] -> [String]
process_strings [] = []
process_strings (hd : tl) = ("Hello, " ++ hd ++ "!") : (process_strings tl)

process :: (a -> a) -> [a] -> [a]
process _ [] = []
process f (hd : tl) = (f hd) : (process f tl)

process' :: (a -> b) -> [a] -> [b]
process' _ [] = []
process' f (hd : tl) = (f hd) : (process' f tl)

who_passed :: [(String, Int)] -> [String]
who_passed [] = []
who_passed ((name, grade) : tl) = if grade >= 5 then
                                    name : who_passed tl
                                  else
                                    who_passed tl

who_passed' :: [(String, Int)] -> [(String, Int)]
who_passed' [] = []
who_passed' ((name, grade) : tl) = if grade >= 5 then
                                    (name, grade) : who_passed' tl
                                  else
                                    who_passed' tl

who_passed'' list = filter (\ (name, grade) -> grade >= 5) list
who_passed''' = filter (\ (name, grade) -> grade >= 5)

who_failed' :: [(String, Int)] -> [(String, Int)]
who_failed' [] = []
who_failed' ((name, grade) : tl) = if grade < 5 then
                                    (name, grade) : who_failed' tl
                                  else
                                    who_failed' tl

who_passed_nice :: [(String, Int)] -> [String]
--who_passed_nice list = map (\(name, _) -> name)
--  (filter (\(name, grade) -> grade >= 5) list)

who_passed_nice = (map fst) . (filter (\(_, grade) -> grade >= 5))

count :: [Int] -> Int
count [] = 0
count (hd : tl) = 1 + count tl

sumlist :: [Int] -> Int
sumlist [] = 0
sumlist (hd : tl) = hd + sumlist tl

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (hd : tl) = if p hd then
                        hd : filter' p tl
                      else
                        filter' p tl

reduce :: [a] -> (a -> b -> b) -> b -> b
reduce [] _ initial = initial
reduce (hd : tl) f initial = f hd (reduce tl f initial)

-- ghci> :t foldl
-- :t foldl
-- foldl :: (b -> a -> b) -> b -> [a] -> b

-- foldl f i [ e1, e2, ..., en ] =
-- f (f (f (f i e1) e2) e3 ...) en

-- ghci> :t foldr
-- :t foldr
-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- foldr f i [ e1, e2, ..., en ] =
-- f e1 (... f en-2 (f en-1 (f en i)))

