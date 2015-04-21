import Common
import Debug.Trace (trace)
import GHC.Exts
-- The proper divisors of a number are all the divisors excluding the number itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As the sum of these divisors is equal to 28, we call it a perfect number.

-- Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors of 284 is 220, forming a chain of two numbers. For this reason, 220 and 284 are called an amicable pair.

-- Perhaps less well known are longer chains. For example, starting with 12496, we form a chain of five numbers:

-- 12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)

-- Since this chain returns to its starting point, it is called an amicable chain.

-- Find the smallest member of the longest amicable chain with no element exceeding one million.

sumOfDivisors = sum . factors


amicableNumberChain :: [Int] -> [Int]
amicableNumberChain [1] = []
amicableNumberChain [x] = amicableNumberChain $ [x] ++ [sumOfDivisors x]
amicableNumberChain (x:xs)
    -- | trace ("" ++ show (x, xs)) False = undefined 	
    | fin > 1000000 = []
 	| fin == 1 = []
	| x == fin = x:init xs -- We have a chain
	| elem fin $ init xs = [] -- We have a loop
	| otherwise = amicableNumberChain $ x:xs ++ [sumOfDivisors fin]
	where fin = last xs

answer s e = sortWith (\a -> 0 - length a) $ filter (/=[]) $ map amicableNumberChain [[a] | a <- [s..e]]