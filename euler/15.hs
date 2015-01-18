import Common
import Debug.Trace (trace)



-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.


-- How many such routes are there through a 20×20 grid?


countSteps :: (Show a, Integral a) => a -> a -> a -> a
countSteps maxSize x y
--    | trace ("" ++ show (maxSize, x , y)) False = undefined
    | x == (maxSize -1) && x == y = 1
    | x == maxSize = 0
    | y == maxSize = 0
    | otherwise = countSteps maxSize (x+1) y + countSteps maxSize x (y+1)



-- New wa to calculate

p = replicate 6 1

doIt :: (Integral a) => [a] -> [a] -> [a]
doIt [] ys =reverse ys
doIt (x:xs) [] = doIt xs [1]
doIt (x:xs) (y:ys) = doIt xs ((x+y) : y : ys)

doItAll :: (Integral a) => Int -> [[a]] -> [[a]]
doItAll maxSize [] = doItAll maxSize [replicate maxSize 1]
doItAll maxSize (x:xs)
    | length xs < (maxSize -1) = doItAll maxSize $ doIt x [] : x : xs
    | otherwise = x:xs

result a = (last.head) $ doItAll a []