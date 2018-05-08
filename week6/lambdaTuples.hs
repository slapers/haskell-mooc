module LambdaTuples where

-- Tuples expressed as pure functions
--
-- So this tuple and its elements
myTup ::(Integer, String)
myTup = (1, "a")

el1 = fst myTup
el2 = snd myTup

--
-- () can be expressed as a function application
mkTup :: a -> b -> (a -> b -> c) -> c
mkTup x y fn = fn x y

-- we can now create a tuple by partially applying mkTup
myTup' :: (Integer -> String -> c) -> c
myTup' = mkTup 1 "a"

-- Here are our tuple accessors
fst' t = t $ \x _y -> x
snd' t = t $ \_x y -> y

-- Lets get the first element of our tuple
el1' = fst' myTup'
el2' = snd' myTup'
