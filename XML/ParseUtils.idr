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
import Lightyear.Char
import Lightyear.Strings

%access public

-- -------------------------------------------------------------- [ Characters ]

word : Parser String
word = map pack (some $ alphaNum) <?> "Word"

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
pathChar = urlChar <|> alphaNum <?> "Path Char"

||| Parse URIs
url : Parser String
url = map pack (some pathChar) <?> "URL"


allowedChar : Parser Char
allowedChar = urlChar <|> alphaNum <|> reservedChar <?> "Reserved Char"

||| Parse XML Words
xmlWord : Parser String
xmlWord = map pack (some entities)
      <|> map pack (some allowedChar)
      <|> word <?> "Valid XML words"

-- ---------------------------------------------------------------- [ KV Pairs ]
expValue : String -> Parser ()
expValue s = skip $ dquote $ string s <* spaces

genKVPair : Parser a -> Parser b -> Parser (a, b)
genKVPair key value = do
    k <- key
    equals
    v <- value <* spaces
    pure (k,v)
  <?> "KV Pair Impl"

keyvalue : Parser (String, String)
keyvalue = do
    (k,v) <- genKVPair (word <* spaces) (quoted '\"')
    pure (k, v)
  <?> "String KV Pair"

keyvalue' : String -> Parser a -> Parser a
keyvalue' key value = do
    (_,v) <- genKVPair (token key) value
    spaces
    pure v
  <?> "Value from Known Key"

-- --------------------------------------------------------------------- [ EOF ]
