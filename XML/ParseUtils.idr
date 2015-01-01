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

eol : Parser ()
eol = char '\n'

anyChar : Parser Char
anyChar = satisfy (const True)

word : Parser String
word = map pack (some $ satisfy isAlphaNum) <?> "Word"

literallyBetween : Char -> Parser String
literallyBetween c = map pack $ between (char c) (char c) (some (satisfy (/= c)))

manyTill : Monad m => ParserT m String a
                   -> ParserT m String b
                   -> ParserT m String (List a)
manyTill p end = scan
  where
    scan = do { end; return List.Nil } <|>
           do { x <- p; xs <- scan; return (x::xs)}

-- --------------------------------------------------------------------- [ EOF ]
