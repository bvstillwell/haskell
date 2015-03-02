import Common
import Data.Ratio

nextEDenominator :: [Integer] -> Integer
nextEDenominator [] = 2
nextEDenominator [2] = 1
nextEDenominator [2,1] = 2
nextEDenominator [2,1,2] = 1
nextEDenominator [_,_] = error "Smash"
nextEDenominator xs
    | a > 1 = 1
    | b > 1 = 1
    | otherwise = c + 2
    where (a:b:c:ys) = reverse xs

nextEList :: [Integer] -> [Integer]
nextEList (xs) = xs ++ [nextEDenominator xs]


doCalc :: (Fractional a) => [Integer] -> a
doCalc [x,y] = fromIntegral x + 1 / fromIntegral y
doCalc (x:y:xs) = fromIntegral x + 1 / doCalc (y:xs)


numeratorAnswer = numerator $ doCalc $ last $ take 101 $ iterate nextEList []
answer = sum $ digitise numeratorAnswer 10