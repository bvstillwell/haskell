--Problem 2



fib :: (Integral a) => [a] -> a
fib [] = 1
fib [1] = 2
fib a = x + y
    where (x:y:xs) = reverse a


--Problem 3
factors :: (Integral a) => a -> [a]
factors a = filter (\x -> mod a x == 0) [2..(truncate . sqrt . fromIntegral) a]
isPrime a = null $ factors a
primeFactors = filter isPrime . factors



--Problem 4

isPalendrome ::  (Eq a) => [a] -> Bool
isPalendrome [] = True
isPalendrome [x] = True
isPalendrome (x:xs)
    | x /= last xs = False
    | otherwise = isPalendrome $ init xs
