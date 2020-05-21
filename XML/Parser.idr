-- -------------------------------------------------------------- [ Parser.idr ]
-- Module      : XML.Parser
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- A DOM-based XML parser.
-- --------------------------------------------------------------------- [ EOH ]
module XML.Parser

import public Text.Lexer
import public Text.Parser

import public Commons.Text.Lexer.Run
import public Commons.Text.Parser.Support
import public Commons.Text.Parser.Run

import XML.Lexer
import XML.DOM

%access private
%default total

-- --------------------------------------------------------------- [ Utilities ]

public export
Parser : (a : Type) -> Type
Parser = Rule Token

ParserE : (a : Type) -> Type
ParserE = RuleEmpty Token

eoi : RuleEmpty Token ()
eoi = eoi isEOI

  where
    isEOI : Token -> Bool
    isEOI EndInput = True
    isEOI _ = False

struct : String -> Parser ()
struct str =
  match ("Expected Symbol '" ++ str ++ "'")
        ()
        (Struct str)

isWord : String -> Parser ()
isWord str =
  match ("Expected Symbol '" ++ str ++ "'")
        ()
        (Word str)

isText : String -> Parser ()
isText str =
  match ("Expected Text '" ++ str ++ "'")
        ()
        (Text str)

isLit : String -> Parser ()
isLit str =
  match ("Expected Lit '" ++ str ++ "'")
        ()
        (StringLit ("\"" ++ str ++ "\""))

isSymbol : String -> Parser ()
isSymbol str =
  match ("Expected Symbol '" ++ str ++ "'")
        ()
        (Symbol str)

entity : Parser String
entity = terminalF "Expected entity"
                   (\x => case tok x of
                            (Entity s) => Just s
                            _          => Nothing)

symbol : Parser String
symbol = terminalF "Expected symbol"
                 (\x => case tok x of
                          (Symbol s) => Just s
                          _          => Nothing)

word : Parser String
word = terminalF "Expected word"
                 (\x => case tok x of
                          (Word s) => Just s
                          _        => Nothing)

textStr : Parser String
textStr = terminalF "Expected text"
                 (\x => case tok x of
                          (Text s) => Just s
                          _        => Nothing)

commentStr : Parser String
commentStr = terminalF "Expected comment"
                    (\x => case tok x of
                            (Comment s) => Just s
                            _           => Nothing)

cdataStr : Parser String
cdataStr = terminalF "Expected doc"
                  (\x => case tok x of
                           (CData s) =>
                             let s'  = substr 9 (length s) s in
                             let s'' = substr 3 (length s') (reverse s')
                             in Just (reverse s'')

                           _         => Nothing)

strLit : Parser String
strLit = terminalF "Expected string literal"
             (\x => case tok x of
                         StringLit s => Just (fst $ span (not . (==) '"') s)
                         _ => Nothing)

kvpair : Parser a -> Parser b -> Parser (a, b)
kvpair key value = do
    k <- key
    isSymbol "="
    v <- value
    pure (k,v)

kvpair' : String -> Parser b -> Parser b
kvpair' key value = do
    isWord key
    isSymbol "="
    v <- value
    pure v

angles : Parser a -> Parser a
angles = between (struct "<") (struct ">")

-- ------------------------------------------------------------------- [ Utils ]


qname : Parser $ Document QNAME
qname = do
    pre <- optional $ (word <* isSymbol ":")
    name <- word
    pure (mkQName name Nothing pre)


attr : Parser (Document QNAME, String)
attr = do
  res <- kvpair qname strLit
  pure (fst res, snd res)

elemStart : Parser (Document QNAME, Document QNAME, (List (Document QNAME, String)))
elemStart = do
  struct "<"
  qn <- qname
  ns <- optional $ kvpair' "xmlns" strLit
  as <- optional $ some attr
  pure (qn, setNameSpace ns qn, fromMaybe Nil as)

elemEnd : Document QNAME -> Parser ()
elemEnd n = angles (struct "/" *> isValid)
  where
    isValid : Parser ()
    isValid = do
       seen <- qname
       if (assert_total $ eqDoc n seen)
         then pure ()
         else fail "Malformed"

-- ------------------------------------------------------------------- [ Nodes ]

export
comment : Parser $ Document COMMENT
comment = map mkComment commentStr

export
cdata : Parser (Document CDATA)
cdata = map mkCData cdataStr

text : Parser (Document TEXT)
text = map (\xs => mkText (concat xs)) $ some text'
   where
     text' : Parser String
     text' =  entity
          <|> symbol
          <|> word
          <|> textStr

export
instruction : Parser $ Document INSTRUCTION
instruction = do
  struct "<"
  isSymbol "?"
  t <- word
  d <- some (kvpair word strLit) -- @TODO make proper URL parser
  isSymbol "?"
  struct ">"
  pure (mkInstruction t d)

export
empty : Parser (Document ELEMENT)
empty = do
  start <- elemStart
  struct "/"
  struct ">"
  pure (mkElement (fst $ snd start) (snd $ snd start) Nil)

private
buildNodeList : List (ty ** prf : ValidNode ty ** Document ty)
             -> (ts ** prfs ** NodeList ts prfs)
buildNodeList [] = ([] ** [] ** [])
buildNodeList ((ty ** prf ** node) :: xs) =
    let (ts ** prfs ** nodes) = buildNodeList xs
     in (ty :: ts ** prf :: prfs ** node :: nodes)


mutual

  export covering
  node : Parser $ (ty ** prf : ValidNode ty ** Document ty)
  node = map (\n => (COMMENT     ** ValidDoc   ** n)) comment
     <|> map (\n => (CDATA       ** ValidCData ** n)) cdata
     <|> map (\n => (INSTRUCTION ** ValidInstr ** n)) instruction
     <|> map (\n => (ELEMENT     ** ValidElem  ** n)) empty
     <|> map (\n => (ELEMENT     ** ValidElem  ** n)) element
     <|> map (\n => (TEXT        ** ValidText  ** n)) text

  covering
  element : Parser (Document ELEMENT)
  element = do
    start <- elemStart
    let qn = (fst $ snd start)
    let as = (snd $ snd start)
    struct ">"
    ns <- many node
    elemEnd (fst start)
    let nodes = buildNodeList ns
    pure (mkElement qn as (snd $ snd nodes))

%default covering

isStandalone : Parser Bool
isStandalone = (isLit "yes" <|> isLit "true") *> pure True

notStandalone : Parser Bool
notStandalone = (isLit "false" <|> isLit "no") *> pure False

export
xmlinfo : Parser (Document INFO)
xmlinfo = do
  struct "<"
  isSymbol "?"
  isWord "xml"
  vers  <- kvpair' "version" strLit
  enc   <- option "UTF-8" $ kvpair' ("encoding")   strLit
  alone <- option True    $ kvpair' ("standalone") (isStandalone <|> notStandalone)
  isSymbol "?"
  struct ">"
  pure (mkXMLInfo vers enc alone)

pubident : Parser (Document IDENT)
pubident = do
  isWord "PUBLIC"
  loc <- strLit
  loc' <- strLit
  pure (mkPublicID loc loc')

sysident : Parser (Document IDENT)
sysident = map mkSystemID (isWord "SYSTEM" *> strLit)

ident : Parser $ Document IDENT
ident = pubident <|> sysident

export
doctype : Parser (Document DOCTYPE)
doctype = do
  struct "<"
  isSymbol "!"
  isWord "DOCTYPE"
  v <- word
  id <- optional ident
  struct ">"
  pure (mkDocType v id)


xmlSnippet : Parser $ Document ELEMENT
xmlSnippet = (element <|> empty)

export
parseXMLSnippet : (str : String) -> Either (Run.ParseError Token) (Document ELEMENT)
parseXMLSnippet = parseString XMLLexer xmlSnippet

export
xmldoc : Parser $ Document DOCUMENT
xmldoc = do
    info  <- xmlinfo
    dtype <- optional doctype
    is    <- many instruction
    doc   <- optional comment
    root  <- element -- Add check if docttype exists for name of root element
    pure $ mkDocument info dtype is doc root

export
parseXMLDoc : (doc : String)
           -> (Either (Run.ParseError Token) (Document DOCUMENT))
parseXMLDoc = parseString XMLLexer xmldoc


export
parseXMLDocFile : (fname : String)
               -> IO (Either (Run.ParseError Token) (Document DOCUMENT))
parseXMLDocFile = parseFile XMLLexer xmldoc

-- --------------------------------------------------------------------- [ EOF ]
