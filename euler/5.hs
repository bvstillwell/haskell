--2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

--What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?


orderedFactors :: (Integral a) => a -> [a]
orderedFactors 0 = []
orderedFactors a = 1 : takeWhile (\x -> let (d, r) = divMod a x in r == 0) [2..]
--orderedFactors a = 1 : takeWhile (\x -> let (d, r) = divMod a x in r == 0 && even d) [2..20]


goodAnswer maxFactor value = (length . orderedFactors) value >= maxFactor

smallestAnswer maxFactor = take 1 $ dropWhile (not . goodAnswer maxFactor) [0,maxFactor..]

--take 1 $ dropWhile (\x -> not $ goodAnswer x) [2520,2570..200000000]