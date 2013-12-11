module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

-- Returns an error message
err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

-- Recursively sequence over m and cons the results, return [] if Nothing
iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

-- Cons two values
cons(a, b) = a:b

-- Ignore the first result
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

-- Ignore the second result
(#-) :: Parser a -> Parser b -> Parser a
m #- n =  m # n >-> fst

-- Parse a sequence of whitespaces
spaces :: Parser String
spaces = iter (char ? isSpace)

-- Parse a token, throw away trailing spaces
token :: Parser a -> Parser a
token m = m #- spaces

-- Parse a letter
letter :: Parser Char
letter =  char ? isAlpha

-- Parse a word by parsing first a letter, then 
-- by a sequence of letters while consing on the 
-- resulting value, tokenize the parser to throw 
-- away trailing whitespaces
word :: Parser String
word = token (letter # iter letter >-> cons)

-- Parse for a sequence of chars with length 'n'
chars :: Int -> Parser String
chars 0 = return []
chars n = char #chars (n-1) >-> cons

-- Parse for a specific substring
accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- Parse for the requirement of a substring
require :: String -> Parser String
require w  = accept w ! (err ("Program error: expecting " ++ w))

-- Parse for a specific character
lit :: Char -> Parser Char
lit c = token char ? (==c)

-- Parse for a digit
digit :: Parser Char 
digit = char ? isDigit 

-- Parse for a digit that also extracts the digit
digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

-- Parse for a number to append to previous number
number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
            
-- Parse for numbers
number :: Parser Integer
number = token (digitVal #> number')

