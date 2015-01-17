--


sumSquares a = sum $ map (^2) [1..a]
sumSumSquares a = (^2) $ sum [1..a]
aDiff a = sumSumSquares a - sumSquares a