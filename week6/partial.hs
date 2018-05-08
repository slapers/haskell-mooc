module Partial where
--
-- This function is partial since it does not work
-- on all Integers, giving a negative integer will
-- not work
partial :: Integer -> Bool
partial x
  | x >= 0 = True

--
-- This function is curried because we feed in arguments
-- by applying only one each time
curried :: Integer -> Integer -> Integer
curried = \x -> \y -> x + y

--
-- This function is the same as above
sameCurried :: Integer -> Integer -> Integer
sameCurried = \x y -> x + y

--
-- This function is the same as above
alsoSame :: Integer -> Integer -> Integer
alsoSame = (+)

--
-- This function is partially applied
partialApplied :: Integer -> Integer
partialApplied = curried 1
