chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

nunLongChains' :: Int
nunLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))



--import System.Random
main = print (
    [5,4..1],
    zipWith (\a b -> (a * 30 + 3 ) / b) [5,4..1] [1,2..5]
    )