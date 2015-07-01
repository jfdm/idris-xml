-- ---------------------------------------------------------- [ ParseUtils.idr ]
-- Module      : XML.ParseUtils
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- Utility parsing things. I should really add them to Lightyear as I
-- use them every where.
-- -------------------------------------------------------------------- [ EOH ]
module XML.ParseUtils

import Lightyear
import Lightyear.Strings

%access public

-- -------------------------------------------------------------- [ Characters ]
eol : Parser ()
eol = char '\n' *> return () <?> "EOL"

anyChar : Parser Char
anyChar = satisfy (const True) <?> "Any Char"

word : Parser String
word = map pack (some $ satisfy isAlphaNum) <?> "Word"

entities : Parser Char
entities = do
    char '&'
    ty <- word
    char ';'
    case ty of
      "quot"    => pure '"'
      "amp"     => pure '&'
      "apos"    => pure '\''
      "lt"      => pure '<'
      "gt"      => pure '>'
      otherwise => satisfy (const False)
  <?> "entities"

reservedChar : Parser Char
reservedChar = do
    c <- satisfy (const True)
    case c of
      '"'       => satisfy (const False)
      '&'       => satisfy (const False)
      '\''      => satisfy (const False)
      '<'       => satisfy (const False)
      '>'       => satisfy (const False)
      otherwise => pure c
  <?> "reserved characters"

private
urlChar : Parser Char
urlChar = do
  c <- satisfy (const True)
  case c of
    '\\'      => pure '\\'
    '/'       => pure '/'
    '.'       => pure '.'
    ':'       => pure ':'
    '#'       => pure '#'
    '='       => pure '='
    '?'       => pure '?'
    '-'       => pure '-'
    otherwise => satisfy (const False)

private
pathChar : Parser Char
pathChar = urlChar <|> satisfy isAlphaNum <?> "Path Char"

||| Parse URIs
url : Parser String
url = map pack (some pathChar) <?> "URL"


allowedChar : Parser Char
allowedChar = urlChar <|> satisfy isAlphaNum <|> reservedChar <?> "Reserved Char"

||| Parse XML Words
xmlWord : Parser String
xmlWord = map pack (some entities)
      <|> map pack (some allowedChar)
      <|> word <?> "Valid XML words"

-- ------------------------------------------------------------- [ Combinators ]

literallyBetween : Char -> Parser String
literallyBetween c = map pack $ between (char c) (char c) (some (satisfy (/= c)))

manyTill : Monad m => ParserT m String a
                   -> ParserT m String b
                   -> ParserT m String (List a)
manyTill p end = scan
  where
    scan : Monad m => ParserT m String (List a)
    scan = do { end; return List.Nil } <|>
           do { x <- p; xs <- scan; return (x::xs)}


-- ---------------------------------------------------------------- [ KV Pairs ]
expValue : String -> Parser ()
expValue s = skip $ dquote $ string s <* space

genKVPair : Parser a -> Parser b -> Parser (a, b)
genKVPair key value = do
    k <- key
    equals
    v <- value <* space
    pure (k,v)
  <?> "KV Pair Impl"

keyvalue : Parser (String, String)
keyvalue = do
    (k,v) <- genKVPair (word <* space) (literallyBetween '\"')
    pure (k, v)
  <?> "String KV Pair"

keyvalue' : String -> Parser a -> Parser a
keyvalue' key value = do
    (_,v) <- genKVPair (token key) value
    space
    pure v
  <?> "Value from Known Key"

-- --------------------------------------------------------------------- [ EOF ]
