module XML.Lexer

import Text.Lexer

import public Commons.Text.Lexer.Run

%default total
%access private


public export
data Token = Entity String
           | Struct String
           | Symbol String
           | Word String
           | Text String
           | Comment String
           | CData String
           | StringLit String
           | WS String
           | NotRecognised String
           | EndInput

public export
Eq Token where
  (==) (Entity x)         (Entity y)        = x == y
  (==) (Struct x)         (Struct y)        = x == y
  (==) (Symbol x)         (Symbol y)        = x == y
  (==) (Word x)           (Word y)          = x == y
  (==) (Text x)           (Text y)          = x == y
  (==) (Comment x)        (Comment y)       = x == y
  (==) (CData x)          (CData y)         = x == y
  (==) (StringLit x)      (StringLit y)     = x == y
  (==) (WS x)             (WS y)            = x == y
  (==) (NotRecognised x)  (NotRecognised y) = x == y
  (==) EndInput           EndInput          = True
  (==) _ _ = False

public export
Show Token where
  show (Entity s)           =  "(Entity " ++ s ++ ")"
  show (Struct s)           =  "(Struct " ++ s ++ ")"
  show (Symbol s)           =  "(Symbol " ++ s ++ ")"
  show (Word s)             =  "(Word " ++ s ++ ")"
  show (Text s)             =  "(Text " ++ s ++ ")"
  show (Comment s)          =  "(Comment " ++ s ++ ")"
  show (CData s)            =  "(CData " ++ s ++ ")"
  show (StringLit s)        =  "(StringLit " ++ s ++ ")"
  show (WS s)               =  "(WS " ++ s ++ ")"
  show (NotRecognised s)    =  "(NotRecognised " ++ s ++ ")"
  show (EndInput)           =  "(EndInput)"

word : Lexer
word = alphaNums

pathChar : Lexer
pathChar = oneOf "\\.:#=?-!"

url : Lexer
url = some pathChar

validSymbol : Lexer
validSymbol = pathChar --<|> (non (oneOf "\"&'<>"))

entity : Lexer
entity = exact "&"
      <+> ((exact "quot")
       <|> (exact "amp")
       <|> (exact "apos")
       <|> (exact "lt")
       <|> (exact "gt"))
      <+> exact ";"

nodeSymbol : Lexer
nodeSymbol = exact "<" <|> exact ">" <|> exact "/"

comment : Lexer
comment = blockComment (exact "<!--") (exact "-->")

cdata : Lexer
cdata = blockComment (exact "<![CDATA[") (exact "]]>")

xmlChar : Lexer
xmlChar = entity <|> non (oneOf "\"&'<>") <|> space

xmlText : Lexer
xmlText = (entity <|> non (oneOf "\"&'<>")) <+> some xmlChar <+> reject (exact "<")

tokenMap : TokenMap Token
tokenMap =
  [ (space, WS)
  , (comment, Comment)
  , (cdata, CData)
  , (stringLit, StringLit)
  , (nodeSymbol, Struct)
  , (entity, Entity)
  , (validSymbol, Symbol)
  , (word, Word)
  , (xmlText, Text)
  , (any, NotRecognised)
  ]

keep : TokenData Token -> Bool
keep t = case tok t of
  WS _ => False
  _ => True


export
XMLLexer : Lexer Token
XMLLexer = MkLexer tokenMap keep EndInput

export
lexXMLStr : String -> Either LexError (List (TokenData Token))
lexXMLStr = lexString XMLLexer

export
lexXMLFile : String -> IO $ Either LexFail (List (TokenData Token))
lexXMLFile = lexFile XMLLexer
