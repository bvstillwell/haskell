--By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

--What is the 10 001st prime number?


prime x = null $ take 1 $ dropWhile (\y -> mod x y > 0 )  [2..(floor $ sqrt (fromIntegral x))]

result num = take num $ filter prime [2..]