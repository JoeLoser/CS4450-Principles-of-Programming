--square :: Integer -> Integer
--square x = x * x

--smaller :: (Integer, Integer) -> Integer
--smaller (x, y) = if x <= y then x else y

import Prelude
import Data.Char -- for nextlet

-- takes letter of alphabet and returns the letter immediately after it (A is considered after Z)
-- if argument is not a letter, returns the argument unchanged
nextlet :: Char -> Char
nextlet c = 
	if(isAlpha(c)) 
		then if (c == 'z')
			then 'a'
			else succ(c) 
		 --then if (c == 'Z')
			--then 'A'
			--else succ(c)
		else c