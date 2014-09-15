--square :: Integer -> Integer
--square x = x * x

--smaller :: (Integer, Integer) -> Integer
--smaller (x, y) = if x <= y then x else y

import Data.Char -- for nextlet

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

-- date is represented by a tuple (day, month, year) and are all of type Int
-- function age takes two dates
	-- first is birthday and second is current date
-- returns age of individual in whole years
age :: (Int, Int, Int) -> (Int, Int, Int) -> Int
age (birth_day, birth_month, birth_year) (current_day, current_month, current_year)
	| (current_month >= birth_month) && (current_day >= birth_day) = current_year - birth_year
	| (current_month >= birth_month) && (current_day < birth_day)  = current_year - birth_year - 1
	| (current_month <= birth_month) = current_year - birth_year - 1