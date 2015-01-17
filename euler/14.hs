import Common
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Ord
import Data.Maybe
--import Debug.Trace (trace)
trace s f = f

-- The following iterative sequence is defined for the set of positive integers:

-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)

-- Using the rule above and starting with 13, we generate the following sequence:

-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

-- Which starting number, under one million, produces the longest chain?

-- NOTE: Once the chain starts the terms are allowed to go above one million.

collatz :: (Integral a) => a -> a
collatz 0 = error "Cannot Collatz a 0"
collatz a
    | mod a 2 == 0 = div a  2
    | otherwise = 3 * a + 1


flop (Just i ) = i
flop Nothing = error "Can't flop nothing"

--Take in a number to collatz and a Data.Map of the (value, length of chain)
collatzChainLengthWithLookup :: (Show k, Integral k, Ord k) => (k, Map.Map k k) ->  (k, Map.Map k k)
collatzChainLengthWithLookup (1, cLMap) = (1,  cLMap)
collatzChainLengthWithLookup (x, cLMap) =
        if  isNothing lookupCount
        then
            let (count, newLookup) = collatzChainLengthWithLookup (collatz x, cLMap)
            in trace ("Miss " ++ show x) (count +1, Map.insert x (count+1) newLookup)
        else trace ("Hit " ++ show x) (flop lookupCount, cLMap)
    where lookupCount = Map.lookup x cLMap

collatzLengthTable = Map.toList . foldr (\v acc -> snd $! collatzChainLengthWithLookup (v, acc)) Map.empty
theResult = List.maximumBy (comparing snd) . collatzLengthTable


-- theResult [0..99999]
-- theResult [100000..199999]
-- theResult [200000..299999]
-- theResult [300000..399999]
-- theResult [400000..499999]
-- theResult [500000..599999]
-- theResult [600000..699999]
-- theResult [700000..799999]
-- theResult [800000..899999]
-- theResult [900000..999999]

runBatch :: (Integral a) => (a, a, a) -> ([a] -> b) -> [b]
runBatch (start, size, steps) f = map (\step -> let range = [start + (step*size)..start + ((step+1)*size)-1] in f $! range) [0..steps -1]

--outOfMemory start batchSize steps = map(\batchNo -> theResult [start + (batchNo*batchSize)+1..(start + (batchNo+1) *batchSize)] ) [0..steps]
--theBatchAnswer = List.maximumBy (comparing snd) . outOfMemory