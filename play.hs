maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs) = max x (maximum' xs)

rreplicate' :: Int -> a -> [a]
rreplicate' n x
    | n <= 0 = []
    | otherwise = x:rreplicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take'(n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
   | a == x    = True
   | otherwise = elem' a xs

quicksort:: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort[a | a <-xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine = multThree 9

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x:map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x:filter' p xs
    | otherwise = filter' p xs

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = mod x 3829 == 0

sum' = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

sum_compreh = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])


sum_f :: (Num a) => [a] -> a
sum_f xs = foldl (\acc x -> acc + x) 0 xs

sum_f' :: (Num a) => [a] -> a
sum_f' = foldl (+) 0

elem_lf :: (Eq a) => a -> [a] -> Bool
elem_lf y ys = foldl (\acc x -> if x == y then True else False) False ys

map_r :: (a -> b) -> [a] -> [b]
map_r f xs = foldr (\x acc -> f x: acc) [] xs
map_l :: (a -> b) -> [a] -> [b]
map_l f xs = foldl (\acc x -> acc ++ [f x]) [] xs

max' :: (Ord a) => [a] -> a
max' = foldr1 (\x acc -> if x > acc then x else acc)

rev' :: [a] -> [a]
rev' = foldl (\acc x -> x:acc) []

prd' :: (Num a) => [a] -> a
prd' = foldl1 (*)

ftr' :: (a -> Bool) -> [a] -> [a]
ftr' p = foldr (\x acc -> if p x then x:acc else acc) []
