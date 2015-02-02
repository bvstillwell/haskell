import Common
import Debug.Trace (trace)

import qualified Text.Numeral.Language.EN as EN
import Text.Numeral.Grammar.Reified (defaultInflection)

alphabetChars = ['a'..'z'] ++ ['A'..'Z']

filterAlphabetChars :: String -> String
filterAlphabetChars = filter (`elem` alphabetChars)

integerToWords :: Integer-> String
integerToWords a = (\(Just b) -> (init . tail . show) b) $ EN.gb_cardinal defaultInflection a

countAlphabetChars :: Integer -> Int
countAlphabetChars = length . filterAlphabetChars .  integerToWords

result :: [Integer] -> Int
result a = sum $ map countAlphabetChars a

