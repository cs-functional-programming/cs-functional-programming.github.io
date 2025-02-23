import Nat
import qualified Lib.Intreg as I
import Test.QuickCheck
import Control.Monad

convert :: Nat -> Int
convert Zero = 0
convert (Succ x) = (convert x) + 1

convert' :: Int -> Nat
convert' x | x < 0 = error "Horror!"
convert' 0 = Zero
convert' x = Succ (convert' (x - 1))

prop1_nat x = convert (convert' x) == x
prop2_nat x = x < 0 || convert (convert' x) == x
prop3_nat x = x >= 0 ==> (convert (convert' x)) == x
prop4_nat x = x >= 0 ==> collect x $ (convert (convert' x)) == x
prop5_nat x = collect (x < 0) $ x < 0 || (convert (convert' x)) == x
prop6_nat x = collect (x < 0) $ x >= 0 ==> (convert (convert' x)) == x

prop10_nat :: Nat -> Bool
prop10_nat x = convert' (convert x) == x

instance Arbitrary Nat where
  arbitrary = oneof $ map (\x -> return (convert' x)) [0..99] -- return Zero, return (Succ Zero), return (Succ (Succ Zero)) ]

r = (reverse :: [Int] -> [Int])

test1 = r [1, 2, 3] == [3, 2, 1]
test2 = r [] == []
test3 = r [10] == [10]
test4 = r (r [3, 5, 7]) == [3, 5, 7]
test5 l = r (r l) == l

qs :: [Int] -> [Int]
qs [] = []
qs (hd:tl) = (qs (filter (<hd) tl)) ++ [hd] ++ (qs (filter (>=hd) tl))

sorted :: [Int] -> Bool
sorted [] = True
sorted [hd] = True
sorted (hd:hd':tl) = (hd <= hd') && (sorted (hd':tl))

prop1 l = sorted (qs l)

prop2 l = length l == length (qs l)

main :: IO ()
main = do putStrLn "Hello!"
          putStrLn $ show (add two three)
          putStrLn $ show (Succ Zero)
          putStrLn $ show (I.add (I.Cons 3) (I.Cons 6))
--          putStrLn $ show (addAux two three two)

          