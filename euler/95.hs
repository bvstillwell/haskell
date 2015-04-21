import Common
import Data.List




myPrimes = take 1000000 primes


primeFactors :: Integer -> [Integer] -> [Integer]
primeFactors n (x:xs)
    | x > n = []
    | r > 0 = primeFactors n xs
    | otherwise = x : primeFactors d (x:xs)
    where (d, r) = divMod n x

factorsFromPrimeFactors :: [Integer] -> [Integer]
factorsFromPrimeFactors xs =sort $ map product $ (nub.init.tail) $ subsequences xs

factors' :: Integer -> [Integer]
factors' n = 1: (factorsFromPrimeFactors $ primeFactors n myPrimes)

