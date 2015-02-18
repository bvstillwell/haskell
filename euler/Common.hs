module Common (
    flattenCoords
    ,wholeSqrt
    ,factors
    ,triangleNumbers
    ,isPrime
    ,digitise
    ,unDigitise
    ,primes
    )

where

--Turn a number into a list of digits
digitise :: (Integral a) => a -> a -> [a]
digitise bigNumber base
    | bigNumber < base = [bigNumber]
    | otherwise = digitise d base ++ [r]
    where (d, r) = divMod bigNumber base

--Turn a list of digits into a number
unDigitise :: (Integral a) => [a] -> a -> a
unDigitise [] _ = 0
unDigitise xs base=  head xs * (base ^ (length xs -1)) + unDigitise (tail xs) base

--Flatten a list like [(a, b), (c,d)] -> [a,b,c,d]
flattenCoords :: [(a,a)] -> [a]
flattenCoords = foldl (\acc (d, r) -> acc ++ [d, r]) []

--Return the whole number of a sqrt
wholeSqrt :: (Integral a) => a -> a
wholeSqrt x = floor $ sqrt  (fromIntegral x :: Double) --Sqrt takes a double, so return x as a doule

--Return the factors of a number
factors :: (Show a, Integral a) => a -> [a]
factors 0 = []
factors x =  1 : flattenCoords [(y, d) | y <- [2..wholeSqrt x], let (d, r) = divMod x y, r==0]

--Return a list of triangle numbers
triangleNumbers :: (Integral a) => [a]
triangleNumbers = scanl (+) 1 [2..]

--Return whether a value is prime or not
isPrime :: (Integral a) => a -> Bool
isPrime x = null $ take 1 $ dropWhile (\y -> mod x y > 0 )  (2 : [3,5..wholeSqrt x])

primes :: [Integer]
primes = filter isPrime [2..]
