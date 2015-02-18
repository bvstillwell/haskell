import Common
import Debug.Trace (trace)


isComposite :: Integer -> Bool
isComposite = (>1) . length . factors

composites = filter isComposite [4..]
oddComposites = filter odd composites

isGoldBachs :: Integer -> Bool
isGoldBachs x = not $ null $ take 1 [ (value, prime, square) | prime <- takeWhile (<x) primes, square <- [1..wholeSqrt x], let value = prime + 2 * (square ^ 2), value == x]

answer :: Integer
answer = head $ dropWhile isGoldBachs oddComposites