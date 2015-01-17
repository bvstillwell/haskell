data B = O
    | X

instance Eq B where
    O == O = True
    X == X = True
    _ == _ = False

--addBits :: [B] -> [B] -> [B]
--addBits (a:as) (b:bs) = xorBL  [a, b, carry as bs]

carry :: [B] -> [B] -> B
carry (O:_) (O:_) = O
carry (x:xs) (y:ys) =
        let c = carry xs ys
        in xorBL [andBL [x, y],  andBL [x, c],  andBL [c, y]]


xorBL :: [B] -> B
xorBL x =
    (length filter (==X) x) % 2

andBL :: [B] -> B
andBL [] = X
andBL (O:_) = O
andBL (X:xs) = andBL xs




main :: IO ()
main = print "Start"