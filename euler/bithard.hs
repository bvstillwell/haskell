import qualified Data.Set as S



data Bit = X | O | V Int deriving (Eq, Ord, Show)

data AndSet = AB Bit | AS (S.Set Bit) deriving (Show)
data XorSet = XB Bit | XS (S.Set AndSet) | XAS (S.Set AndSet) deriving (Show)

newtype NewTypeInt = N Int
	deriving (Eq, Ord, Show)

--instance Eq Bit where
--	(V a) == (V b) = a == b
--	X == X = True
--	O == O = True
--	_ == _ = False

-- turn 2 bits into a set
-- posibilities
-- {} 
-- {X}
-- {Vn, Vn+1, ... X}
--and' :: Bit -> Bit -> S.Set Bit
--and' O _ = S.empty
--and' _ O = S.empty
--and' a b = S.fromList [a,b]

-- turn 2 bits into a set
-- Possibilites
-- {}
-- {X}
-- {Vn, Vn+1, ... X}
--xor' :: Bit -> Bit -> S.Set Bit
--xor' a b
--	| a == b = S.empty
--	| a == O = S.singleton b
--	| b == O = S.singleton a
--	| otherwise = S.fromList [a, b]


--and' :: Bit -> S.Set Bit -> S.Set Bit
--and' O _ = S.empty
--and' a b = S.insert a b

xor' :: Bit -> S.Set Bit -> S.Set Bit
xor' O b = b
xor' a b
	| S.member a b = S.delete a b
	| otherwise = S.insert a b


--And 2 bits together
andBitBit :: Bit -> Bit -> AndSet
andBitBit O _ = AB O --Minimise Always minimise O
andBitBit _ O = AB O
andBitBit a X = AB a --don't care about X's
andBitBit X b = AB b
andBitBit a b
	| a == b = AB a	
	| otherwise = AS (S.fromList [a, b]) --Promote to a set

--Mix a bit with an and set
andBitSet :: Bit -> AndSet -> AndSet
andBitSet a (AB b) = andBitBit a b -- These are 2 bits
andBitSet X (AS b) = AS b -- X's do nothing
andBitSet a (AS b)
	| S.null b = error "Shouldn't happen"
	| S.size b == 1 = error "Shouldn't happen"
	| otherwise = AS (S.insert a b)

--Mix 2 and sets together
andSetSet :: AndSet -> AndSet -> AndSet
andSetSet (AB a) (AB b) = andBitBit a b
andSetSet (AB a) (AS b) = andBitSet a (AS b)
andSetSet (AS a) (AB b) = andBitSet b (AS a)
andSetSet (AS a) (AS b)
	| S.null a = error "Shouldn't happen"
	| S.size a == 1 = error "Shouldn't happen"
	| S.null b = error "Shouldn't happen"
	| S.size b == 1 = error "Shouldn't happen"
	| otherwise = AS (S.union a b)