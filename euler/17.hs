import Common
import Debug.Trace (trace)
import qualified Text.Numeral.Language.EN as EN
import Text.Numeral.Grammar.Reified (defaultInflection)

-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

alphabetChars = ['a'..'z'] ++ ['A'..'Z']

filterAlphabetChars :: String -> String
filterAlphabetChars = filter (`elem` alphabetChars)

integerToWords :: Integer-> String
integerToWords a = (\(Just b) -> (init . tail . show) b) $ EN.gb_cardinal defaultInflection a

countAlphabetChars :: Integer -> Int
countAlphabetChars = length . filterAlphabetChars .  integerToWords

result :: [Integer] -> Int
result a = sum $ map countAlphabetChars a

