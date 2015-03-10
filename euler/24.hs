import System.IO
import Data.Char (ord, isAlpha)
import Data.List (sort)

wordsDelim :: (Char -> Bool) -> String -> [String]
wordsDelim f s =  case dropWhile f s of
                      "" -> []
                      s' -> w : wordsDelim f s''
                            where (w, s'') = break f s'

nameScore :: String -> Int
nameScore "" = 0
nameScore (x:xs)
    | isAlpha x = (ord x - ord 'A' + 1) + nameScore xs
    | otherwise = nameScore xs

wordScores :: [String] -> Int
wordScores xs = sum [ a * b | (a,b) <- zip (map nameScore $ sort xs) [1..]]

main :: IO ()
main = do
    contents <- readFile "p022_names.txt"
    let aDelim = wordsDelim (==',') contents
        answer = wordScores aDelim
    print answer