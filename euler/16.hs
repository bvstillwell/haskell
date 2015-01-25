import Common
import Debug.Trace (trace)

-- 215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

-- What is the sum of the digits of the number 21000?

theAnswer :: Integer -> Integer
theAnswer n = sum  $ digitise (2^ n) 10