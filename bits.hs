import Data.List


data Variable = V Int

type AndList = [Variable]
type XorList = [AndList]
type InverseXorList = [AndList]

data Var a = Var a deriving (Show, Eq)
data Bit = O | X | XL XorList

type XL = [Var Int] -- Xor list of V
type IXL = [Var Int] -- Inverse Xor List


binarise :: Integer -> [Bit]
binarise n = map (\x -> if x == 1 then X else O) (digitise n 2)

digitise :: Integer -> Integer -> [Integer]
digitise bigNumber base
    | bigNumber < base = [bigNumber]
    | otherwise = digitise d base ++ [r]
    where (d, r) = divMod bigNumber base

bRotateRight :: Int -> [Bit] -> [Bit]
bRotateRight _ [] = []
bRotateRight 0 xs = xs
bRotateRight n xs = bRotateRight (n-1) (last xs : init xs)

bRotateLeft :: Int -> [Bit] -> [Bit]
bRotateLeft _ [] = []
bRotateLeft 0 xs = xs
bRotateLeft n (x:xs) = bRotateLeft (n-1) (xs ++ [x])

bShiftRight :: Int -> [Bit] -> [Bit]
bShiftRight _ [] = []
bShiftRight 0 xs = xs
bShiftRight n xs = O :(bShiftRight (n-1) $ init xs)

bXor = zipWith (bXor')
bXor' :: Bit -> Bit -> Bit
bXor' O x = x
bXor' a b
	| a == b 	= O
	| otherwise = X

bAnd = zipWith (bAnd')
bAnd' :: Bit -> Bit -> Bit
bAnd' X X = X
bAnd' _ _ = O

bNot' :: Bit -> Bit
bNot' O = X
bNot' X = O
--bNot' (V n) = [(V n)]