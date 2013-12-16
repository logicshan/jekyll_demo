type Parser a = String -> [(a, String)]

runParser :: Parser a -> String -> [(a, String)]
runParser p str = p str

-- Primitive parsers

failp :: Parser a
failp = \str -> []

succp :: a -> Parser a
succp x = \str -> [(x, str)]

symp :: Char -> Parser Char
symp x = \str -> case str of
  [] -> []
  c:cs -> [(c,cs) | c == x]

-- Sequential composition
infixr 6 <*>
(<*>) :: Parser a -> Parser b -> Parser (a,b)
p1 <*> p2 = \str -> [((v1, v2), cs2) | (v1, cs1) <- p1 str,
                                       (v2, cs2) <- p2 str]

-- Parallel composition
infixr 4 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = \str -> p1 str ++ p2 str

-- Manipulating values
infixr 5 <@
(<@) :: Parser a -> (a -> b) -> Parser b
p <@ f = \str -> [(f v, cs) | (v, cs) <- p str]