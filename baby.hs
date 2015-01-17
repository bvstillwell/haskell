doubleMe x = x + x

--doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y

--doubleSmallNumber x = if x > 100 then x else x * 2
doubleSmallNumber' x = (if x > 100 then x else x * 2) +1



--removeNonUpperCase :: String -> String
removeNonUpperCase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

--addThree :: Int -> Int -> Int -> Int
addThree a b c = a+b+c

factorial' :: Int -> Int
factorial' a = product [1..a]

lucky :: (Integral a, Show a) => a -> String
lucky 7 = "Lucky number seven"
lucky x = "Bad luck" ++ show x

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Blahg"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

capital :: String -> String
capital "" = "Empty String"
capital all@(x:xs) = "The capital of " ++ all ++ " is " ++ [x]

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a == b = EQ
    | a < b = LT
    | otherwise = GT

initials :: String -> String -> String
initials first last = [f] ++ ". " ++ [l]
    where
        (f:_) = first
        (l:_) = last

test :: (Num a, Ord a) => [a] -> a
test [] = 0
test (a:[]) = a
test (a:_:[])
    | a < 0 = -10
    | otherwise = c
    where c = 4 * a
test (x:xs) = test xs

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list"
maximum' [a] = a
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num a, Ord a) => a -> i -> [i]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' n _
    | n <=0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' a = a:repeat' a

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs


quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs)  = smaller' ++ [x] ++ bigger'
    where smaller' = quicksort' [ a | a <- xs, a <= x]
          bigger' = quicksort' [a | a <- xs, a > x]

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : a
    | otherwise = a
    where a = filter' f xs

quicksort'' :: (Ord a) => [a] -> [a]
quicksort'' [] = []
quicksort'' (x:xs)  = smaller' ++ [x] ++ bigger'
    where smaller' = quicksort'' (filter (<=x) xs)
          bigger' = quicksort'' (filter (>x) xs)

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x
    | even x = let y = x `div` 2 in x : collatz y
    | otherwise = let y = (x * 3) + 1 in x : collatz y

numLongChains :: Int
numLongChains = sum [1 | a <- [1..100], length (take 16 (collatz a)) > 15]

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ x [] = x
foldl' f x (y:ys) = foldl' f (f x y) ys



maximum'' :: (Ord a) => [a] -> a
maximum'' = foldr1 (\x acc -> if (x > acc) then x else acc)

dot :: (b -> c) -> (a -> b) -> a -> c
f dot g = \x -> f (g x)