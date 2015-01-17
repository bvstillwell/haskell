--mylast :: [a] -> a
--mylast [x] = x
--mylast (x:xs) = head  (reverse xs)

myButLast :: [a] -> a
myButLast [x] = x
myButLast [x, y] = x
myButLast (x:xs) =  myButLast xs

elementAt :: [a] -> Int -> a
elementAt  (x:_) 1 = x
elementAt (_:xs) k
    |   k < 1               = error "Element out of bounds"
    |   otherwise     = elementAt xs (k-1)

myLength :: [a] -> Int
myLength [] = 0
myLength [a] = 1
myLength (x:xs) = 1 + myLength xs
--main = print [
--    myLength [123, 456, 789] ,
--    myLength "Hello, world!"
--    ]

myReverse  :: [a] -> [a]
myReverse  list = myReverse' list []
    where
        myReverse' [] reversed                      = reversed
        myReverse' (x:xs) reversed               = myReverse' xs (x:reversed)
--main = print (
--    myReverse "A man, a plan, a canal, panama!",
--    myReverse [1,2,3,4]
--    )

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs
--main = print (
--    isPalindrome [1,2,3], isPalindrome "madamimadam", isPalindrome [1,2,4,8,16,8,4,2,1]
--    )

data NestedList a = Elem a | List [NestedList a]

flatten  :: NestedList a -> [a]
flatten  (Elem x) = [x]
flatten  (List x) = concatMap flatten x
--main = print (
    ----flatten (Elem 5),
    ----flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    --flatten (List [])
    --)

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (a:b:xs)
    |   a == b              = compress (a:xs)
    |   otherwise       = a:compress (b:xs)
main = print (
    compress "aaaabccaadeeee"
    )

pack :: (Eq a) => [a] -> [a]
pack [] = []
pack [x] = [x]
pack (a:b:xs)
    |   a == b              = compress (a:xs)
    |   otherwise       = a:compress (b:xs)
main = print (
     [x | x <- [1..1000],  mod x 5 == 0 || mod x 3 ==0]
    )

