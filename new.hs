lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky otherwise = "Sorry, you're out of luck, pal!"

addVectors :: (Num a) => (a, a) -> (a, a)  -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors2 :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)



xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
z = [a+b | (a,b) <- xs]


max' :: (Ord a) => a -> a -> a
max' a b
    |   a > b = a
    |   otherwise = b


max'' :: (Ord a) => [a] -> a
max'' [] = error "maximum of empty list"
max'' [x] = x
max'' (x:xs)
    |   x > maxTail = x
    |   otherwise = maxTail
    where maxTail = max'' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    |   n <= 0           = []
    |   otherwise   = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0  = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

repeat' :: a -> [a]
repeat' x = x:repeat' x

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    |   a == x = True
    |   otherwise = elem' a xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort' [a | a <- xs, a <= x] ++ [x] ++ quicksort' [a | a <- xs, a > x]

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwise :: (a -> a) -> a -> a
applyTwise f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

nunLongChains :: Int
nunLongChains = length (filter isLong (map chain [1..100])))
    where isLong xs = length xs > 15

nunLongChains' :: Int
nunLongChains' = length (filter (\xs -> length xs > 15))

--import System.Random
main = print (
    nunLongChains
    )