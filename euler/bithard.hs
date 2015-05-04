module BitHard (    
    notExpr,
    andBitBit,
    andExprExpr,
    xorBitBit,
    Bit(X,O,V),
    Expr(B,AS,XS),
    (&&)
    )
where

import qualified Data.Set as S
import Data.Boolean

data Bit = X | O | V Int deriving (Eq, Ord)

instance Show Bit where
  show X = "X"
  show O = "O"
  show (V n) = "V"++show n


data Expr = B Bit | AS (S.Set Expr) | XS (S.Set Expr)
	deriving (Eq, Ord)

instance Show Expr where
  show (B b) = show b
  show (AS as) = "a" ++ show (S.toList as)
  show (XS xs) = "x" ++ show (S.toList xs)

(a) = andExprExpr
(x) = xorExprExpr

createVars :: Int -> Int -> [Expr]
createVars a b 
	| a > b = []
	| otherwise = (B (V a)) : createVars (a+1) b

-- *****************************
-- NOT
-- *****************************
--Not a bit
notBit :: Bit -> Expr
notBit O = B X
notBit X = B O
notBit (V v) = xorBitBit X (V v)

--Not an expr
notExpr :: Expr -> Expr
notExpr (B a) = notBit a
notExpr s = xorBitExpr X s


-- *****************************
-- AND
-- *****************************
--And 2 bits together
--Complete
andBitBit :: Bit -> Bit -> Expr
andBitBit O _ = B O --Minimise Always minimise O
andBitBit _ O = B O
andBitBit a X = B a --don't care about X's
andBitBit X b = B b
andBitBit a b
	| a == b = B a	
	| otherwise = AS (S.fromList [B a, B b]) --Promote to a set

--Mix a bit with an and set
andBitExpr :: Bit -> Expr -> Expr
andBitExpr O _ = B O
andBitExpr X b = b -- X's do nothing
andBitExpr (V v) (B b) = andBitBit (V v) b
andBitExpr (V v) (AS as) = AS (S.insert (B (V v)) as) --Simple insert
--Here we create a new XS from AND'ing (V n) with each item in the XS
andBitExpr (V v) (XS xs) = foldr1 xorExprExpr $ map (andBitExpr (V v)) (S.toList xs)

andExprExpr :: Expr -> Expr -> Expr
--Bit bit
andExprExpr (B b1) (B b2) = andBitBit b1 b2
--Simple append in most cases
andExprExpr (B b) s = andBitExpr b s
andExprExpr s (B b) = andBitExpr b s
andExprExpr (AS as1) (AS as2) = AS (S.union as1 as2)
--XOR the cartesion product of the sets (AND them)
andExprExpr (XS xs1) (XS xs2) = foldr1 xorExprExpr [andExprExpr x1 x2 | x1<-S.toList xs1, x2<-S.toList xs2]
--Map XS across AS and XOR everything.
andExprExpr (AS as) (XS xs) = foldr1 xorExprExpr $ map (andExprExpr (AS as)) (S.toList xs)
andExprExpr (XS xs) (AS as) = andExprExpr (AS as) (XS xs)



-- *****************************
-- XOR
-- *****************************
--This might be required when items are removed from a list
xorMin' :: Expr -> Expr
xorMin' (XS xs) 
	| size == 0 = B O
	| size == 1 = head $ S.elems xs
	| otherwise = (XS xs)
	where size = S.size xs
	
--Xor 2 bits together
xorBitBit :: Bit -> Bit -> Expr
xorBitBit O a = B a
xorBitBit a O = B a
xorBitBit a b	
	| a == b = B O --Check can't be the same
	| otherwise = XS (S.fromList [B a, B b]) --Promote to a set

xorBitExpr :: Bit -> Expr -> Expr
xorBitExpr O b = b --Ignore O
xorBitExpr b1 (B b2) = xorBitBit b1 b2
xorBitExpr b (XS xs) 
	| S.member (B b) xs = xorMin' $ XS (S.delete (B b) xs) --Xor remove if already in list
	| otherwise = XS (S.insert (B b) xs)
xorBitExpr b (AS as) = XS (S.fromList [(B b), (AS as)])

xorExprExpr :: Expr -> Expr -> Expr
xorExprExpr (B b1) (B b2) = xorBitBit b1 b2
xorExprExpr (B b) s = xorBitExpr b s
xorExprExpr s (B b) = xorBitExpr b s
--XOR for here is the difference
xorExprExpr (XS xs1) (XS xs2) = xorMin' $ XS (S.difference xs1 xs2)	
--Test for equality
xorExprExpr (AS as1) (AS as2)
	| as1 == as2 = B O
	| otherwise = XS (S.fromList [(AS as1), (AS as2)])
--Add the AS to the XS if it doesn't exist
xorExprExpr (AS as) (XS xs)
	| S.member (AS as) xs = xorMin' $ XS (S.delete (AS as) xs) --Xor remove if already in list
	| otherwise = XS (S.insert (AS as) xs)
xorExprExpr (XS xs) (AS as) = xorExprExpr (AS as) (XS xs)
