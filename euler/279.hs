import qualified Data.Angle

combinationsOf3Integrals :: (Integral a) => a -> [(a, a, a)]
combinationsOf3Integrals n = [(a,b,c) | a <- [1..n], b <- [1..a], c <- [1..b], a+b+c < n]

