import Common
import Debug.Trace (trace)
import Data.List

-- The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

-- Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.


isTruncatablePrimeRight :: (Integral a, Show a) => a -> Bool
isTruncatablePrimeRight a
    -- | trace (show a) False = undefined
    | a < 10 = isPrime a
    | otherwise = isPrime a && isTruncatablePrimeRight (read (tail $ show a) :: Integer)

isTruncatablePrimeLeft :: (Integral a, Show a) => a -> Bool
isTruncatablePrimeLeft a
    -- | trace (show a) False = undefined
    | a < 10 = isPrime a
    | otherwise = isPrime a && isTruncatablePrimeLeft (read (init $ show a) :: Integer)

primeList = [2,3,5,7]

getNextPrimes :: Integer -> [Integer]
getNextPrimes a
    -- | trace ("#" ++ show a ++ "#") False = undefined
    | otherwise = [mayBePrime | n <- [1, 3 .. 9], m <- 0 : [1, 3 .. 9], let mayBePrime = read( show m ++ show a  ++ show n) :: Integer, isPrime mayBePrime]

runPrimeList :: [Integer] -> [Integer]
runPrimeList [] = []
runPrimeList (x:xs) = x : runPrimeList newList
    where newList = sort $ nub (xs ++ getNextPrimes x)

answer = nub $ filter (\a -> isTruncatablePrimeRight a && isTruncatablePrimeLeft a && (>10) a) $ runPrimeList primeList