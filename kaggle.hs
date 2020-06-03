import Data.List
-- | examples from python course on kaggle done in haskell

-- | Loops and List comprehensions


-- |1. Return whether the given list of numbers is lucky. A lucky list contains
-- |    at least one number divisible by 7.

-- |	With list comprehensions
has_lucky_number :: (Integral a, Eq a) => [a] -> Bool
has_lucky_number xs = length ([x | x <- xs, x `mod` 7==0]) > 0

-- | 	With pattern matching and recursion
has_lucky_number' :: (Integral a, Eq a) => [a] -> Bool
has_lucky_number' [] = False
has_lucky_number' (x:xs) = if mod x 7 == 0 then True else has_lucky_number' xs

-- |	With Data.List module
has_lucky_number'' :: (Integral a, Eq a) => [a] -> Bool
has_lucky_number'' xs = any (\x -> x `mod` 7 == 0) xs

-- |	With function application or over map in Data.Lists
has_lucky_number''' :: (Integral a, Eq a) => [a] -> Bool
has_lucky_number''' xs = or $ map (\x -> x `mod` 7 == 0) xs

-- |	With function composition or . map in Data.Lists
has_lucky_number'''' :: (Integral a, Eq a) => [a] -> Bool
has_lucky_number'''' xs = or . map (\x -> x `mod` 7 == 0) $ xs

-- | 2. R and Python have some libraries (like numpy and pandas) compare each element of the list to 2 (i.e. do an 'element-wise' comparison)
-- | and give us a list of booleans like [False, False, True, True].
-- | Implement a function that reproduces this behaviour, returning a list of booleans corresponding to whether the corresponding element is greater than n.

elemWise_gt :: (Ord a) => [a] -> a -> [Bool]
elemWise_gt xs t = map (>t) xs