--Joe Loser
--CS4450 HW0
--9/14/14

import Data.Char

-- takes letter of alphabet and returns the letter immediately after it (A is considered after Z)
-- if argument is not a letter, returns the argument unchanged
nextlet :: Char -> Char
nextlet 'z' = 'a'
nextlet 'Z' = 'A'
nextlet c
	| isAlpha(c) = succ(c)
	| otherwise = c

-- converts a digit char (0-9) to its corresponding numerical value
-- if arg isn't a digit characters, returns -1
digitval :: Char -> Int
digitval c
	| isDigit(c) = digitToInt(c)
	| otherwise  = -1

-- accepts two functions (say, f and g) and "twines" them together into a
-- function that accepts a single argument and constructs a tuple of type
-- (b, c) from the result of applying the two functions to the argument
twine :: (a -> b) -> (a -> c) -> a -> (b, c)
twine f g a = (f a, g a)

-- function cond where it returns x if p is true or y otherwise
cond :: Bool -> a -> a -> a
cond p x y 
	| p == True = x
	| otherwise = y

-- date is represented by a tuple (day, month, year)
-- first tuple is birthday and second tuple is current dates
-- returns age of individual in whole years
age :: (Int, Int, Int) -> (Int, Int, Int) -> Int
age (d, m, y) (d', m', y')
      | m' > m = y' - y
      | m' < m = y' - y - 1
      | m' == m && d' >= d = y' - y
      | m' == m && d' < d = y' - y - 1
