module Main where

type LB = [B]

data B = O
    | X
    | V { id :: Integer}
    | A [B]

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

xor2 :: [[B]]-> [[B]]
xor2 [] = []
xor2 (x:xs)
    | null x = xor2 xs
    | x `elem` xs = xor2 (replaceX2 xs x [])
    | otherwise = x:xor2 xs

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

--WOW MOTHA FUKA
replaceX2 :: [[B]] -> [B] -> [B] -> [[B]]
replaceX2 items old new = mapOnce check items where
    check item  | item == old = Just new
                | otherwise   = Nothing

add' :: [B] -> [B] -> [B]
add' [] a = a
add' a [] = a
add' (x:xs) (y:ys)
    | length xs < length ys = add' (O:x:xs) (y:ys)
    | length xs > length ys = add' (x:xs) (O:y:ys)
    | otherwise = [] -- addEven (x:xs) (y:ys)

--addEven :: [B] -> [B] -> B -> [B]
--addEven _ _ = []

---addCarry :: B -> B -> B -> B
--addCarry a b v = bxor (band a b)  (band a c) (band b c)

band :: B -> B -> B
band O _ = O
band _ O = O
band X a = a
band a X = a
band a b
    | a == b = a
    | otherwise = A [a, b] --For V



andBL :: [B] -> B
andBL (O:_) = O
andBL (a:[]) = a
andBL [] = X -- We need And to return a positive result
andBL (X:xs) = andBL xs -- There should be something more interesting in the list
andBL (A x: xs) = andBL (x ++ xs) --Join A lists
andBL (V x:xs) =
    case result of
        O -> O
        X -> V x
        A y -> A (V x:y)
        V y -> if x == y then V x
        otherwise -> A [V x, result]

    where result = andBL xs





main :: IO ()
main = print "Start"