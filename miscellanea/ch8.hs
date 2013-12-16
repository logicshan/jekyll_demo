import Prelude hiding (return, (>>=))
import Data.Char (isDigit, isLower, isUpper, isAlpha, isAlphaNum, isSpace)

type Parser a = String -> [(a, String)]

return :: a -> Parser a
return v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
  "" -> []
  x:xs -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

infixl 1 >>=
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
  [] -> []
  [(v, out)] -> parse (f v) out

pp :: Parser (Char, Char)
pp = item >>= \x ->
     item >>= \_ ->
     item >>= \y ->
     return (x, y)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
  [] -> parse q inp
  [(v, out)] -> [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x -> if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string "" = return ""
string (x:xs) = char x >>= \_ ->
                string xs >>= \_ ->
                return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = p >>= \v ->
          many p >>= \vs ->
          return (v:vs)

ident :: Parser String
ident = lower >>= \x ->
        many alphanum >>= \xs ->
        return (x:xs)

nat :: Parser Int
nat = many1 digit >>= \xs -> return (read xs)

space :: Parser ()
space = many (sat isSpace) >>= \_ -> return ()

token :: Parser a -> Parser a
token p = space >>= \_ ->
          p     >>= \v ->
          space >>= \_ ->
          return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

p :: Parser [Int]
p = symbol "[" >>= \_ ->
    natural >>= \n ->
    many (symbol "," >>= \_ ->
          natural) >>= \ns ->
    symbol "]" >>= \_ ->
    return (n:ns)


expr :: Parser Int
expr = term >>= \t ->
       (symbol "+" >>= \_ ->
        expr >>= \e ->
        return (t + e)) +++ return t

term :: Parser Int
term = factor >>= \f ->
       (symbol "*" >>= \_ ->
        term >>= \t ->         
        return (f * t)) +++ return f

factor :: Parser Int
factor = (symbol "(" >>= \_ ->
         expr >>= \e ->
         symbol ")" >>= \_ ->
         return e) +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
  [(n, [])] -> n
  [(_, out)] -> error $ "unused input " ++ out
  [] -> error "invalid input"