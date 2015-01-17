module Main where

data B = O
    | X
    | V { id :: Integer}
    deriving (Show)--Bit is a 0 or 1 or a Variable with and ID

instance Eq B where
    O == O = True
    X == X = True
    (V a) == (V b) = a == b
    _ == _ = False

--data BL :: [B] deriving (Show)--Bit is a 0 or 1 or a Variable with and ID

--SHIFT RIGHT
shr :: (Num a, Eq a) => a -> [B] -> [B]
shr _ [] = []
shr 0 xs = xs
shr b xs = shr (b-1) (init xs)

--SHIFT LEFT
shl :: (Num a, Eq a) => a -> [B] -> [B]
shl 0 xs = xs
shl b xs = shl (b-1) (xs ++ [O])

--XOR
xor :: [B] -> [B]
xor [] = []
xor (x:xs)
    | x == O = xor xs
    | x `elem` xs = xor (replaceX xs x O)
    | otherwise = x:xor xs

--AND
and' :: [B] -> [B]
and' [] = []
and' (x:xs)
    | O `elem` xs = []
    | otherwise = x:and' (filter (/= x) xs)

--WOW MOTHA FUKA
mapOnce :: (a -> Maybe a) -> [a] -> [a]
mapOnce _ []     = []
mapOnce f (x:xs) = case f x of
        Nothing -> x : mapOnce f xs
        Just y  -> y : xs

--WOW MOTHA FUKA
replaceX :: [B] -> B -> B -> [B]
replaceX items old new = mapOnce check items where
    check item  | item == old = Just new
                | otherwise   = Nothing

main :: IO ()
main = print "Start"