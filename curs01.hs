-- to do any sort of interesting computation
-- we define functions

-- equation
sum_two x y = x + y

my_pi = 3.14

x = x + 1

{-
int x()
{
  return x() + 1;
}
-}

sum_first_n :: Integer -> Integer
sum_first_n 0 = 0
sum_first_n n = n + sum_first_n (n-1)

-- stack: what if we put in -n in the args?

sum_first_n' n = if n <= 0 then
                   0
                 else
                   n + sum_first_n' (n-1)
{-
fails to compile:
sum_first_n'' (-n) = 0
              ^^^^
           we are not allowed function calls
           in patterns
sum_first_n'' 0 = 0
sum_first_n'' n = n + sum_first_n'' (n-1)
-}

{-

is_prime (x * y) = False

is_prime (x * y) = x

get_non_det (x + y) = x

> get_non_det 10
????

-}
