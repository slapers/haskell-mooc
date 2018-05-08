module LambdaLists where

-- Lets see how we can define lists as functions
ls :: [Integer]
ls = [1,2,3]

-- Lists are constructed using the cons operator (:)
-- the last element is an empty list
-- 1:2:3:[]

emptyLs = \_ -> True

cons x xs f = f x xs

ls' = cons 1 (cons 2 (cons 3 emptyLs))

isEmpty lst = lst (\x xs -> False)
