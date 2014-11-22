-- Joe Loser
-- CS4450 HW1
-- 10/4/14

-- 1. Factorial
fact :: Int  -> Int
fact n 
  | (n <= 0) = 1
  | otherwise = product [1..n]

-- 2. nth number in Fibonacci sequence 
fib :: Int -> Int
fib n
  | n <= 0= 0
  | n == 1 = 1
  | n > 1 = fib(n-1) + fib(n-2)

-- 3. Accepts two functions f and g and an int
munge :: (Int->Int) -> (Int -> Int) -> Int -> Int
munge f g x
  | ((x `mod` 2) == 0) = f x
  | otherwise = g x

-- 4. Determine whether monotonically increasing over [a, b]
mono :: (Int->Int) -> Int -> Int -> Bool
mono f a b 
  | (b <= a)  = True
  | f a > f(a + 1) = False
  | otherwise = mono f (a + 1) (b)

-- 5.
runAgain :: Int -> (a -> a) -> (a -> a)
runAgain n f
  | n <= 0 = id 
  -- could also user iterate
  | otherwise = foldr (.) id (replicate n f)

-- 6. Use stutter to call runAgain and print a string n times
stutter :: Int -> String -> String
stutter n words = runAgain n (++ words) words
