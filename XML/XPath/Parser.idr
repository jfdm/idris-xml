-- -------------------------------------------------------------- [ Parser.idr ]
-- Module      : XML.XPath.Parser
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- Turn an xpath dsl instance to edsl
-- --------------------------------------------------------------------- [ EOH ]
module XML.XPath.Parser

import public Text.Lexer
import public Text.Parser

import public Commons.Text.Lexer.Run
import public Commons.Text.Parser.Support
import public Commons.Text.Parser.Run

import XML.DOM

import XML.XPath.Types

%access private

keywords : List String
keywords = ["text", "comment", "cdata"]

symbols : String
symbols = "@()*/"


tokenMap : TokenMap Token
tokenMap =
  [ (space, WS)
  , (oneOf symbols, Symbol)
  ]
  ++
     Functor.map (\x => (exact x, ReservedNode)) keywords
  ++
  [ (alphaNums, UserNode)
  , (any, NotRecog)
  ]

keep : TokenData Token -> Bool
keep t = case tok t of
  WS _ => False
  _ => True

export
XPathLexer : Lexer Token
XPathLexer = MkLexer tokenMap keep EndInput

export
lexXPathStr : String -> Either LexError (List (TokenData Token))
lexXPathStr = lexString XPathLexer

export
lexXPathFile : String -> IO $ Either LexFail (List (TokenData Token))
lexXPathFile = lexFile XPathLexer


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

nodeIs : String -> Parser ()
nodeIs str =
  match ("Expected Symbol '" ++ str ++ "'")
        ()
        (ReservedNode str)

node : Parser String
node = terminalF "Expected node"
                   (\x => case tok x of
                            (UserNode s) => Just s
                            _          => Nothing)

symbol : String -> Parser ()
symbol str =
  match ("Expected Symbol '" ++ str ++ "'")
        ()
        (Symbol str)

parens : Parser ()
parens = symbol "(" *> symbol ")" *> pure ()

-- -------------------------------------------------------------------- [ Test ]

nodeTest : Parser $ (b ** ty ** XPath ty (TEST b))
nodeTest =
      nodeIs "text" *> parens *> pure (_ ** _ ** Text)
  <|> nodeIs "comment" *> parens *> pure (_ ** _ ** Comment)
  <|> nodeIs "cdata" *> parens *> pure (_ ** _ ** CData)
  <|> do symbol "@"
         w <- node
         pure (_ ** _ ** Attr w)

-- ------------------------------------------------------------------- [ Nodes ]

nodeStr : Parser (XPath ELEMENT NODE)
nodeStr =
  do n <- node
     pure (Elem n)

nodeAny : Parser (XPath ELEMENT NODE)
nodeAny = symbol "*" *> pure Any


nodeXPath : Parser (XPath ELEMENT NODE)
nodeXPath = nodeAny <|> nodeStr

-- ------------------------------------------------------------------- [ Roots ]

aRoot : Parser (XPath ELEMENT ROOT)
aRoot = do symbol "/"
           n <- nodeXPath
           pure (Root n)

dRoot : Parser $ XPath ELEMENT ROOT
dRoot = do symbol "/"
           symbol "/"
           n <- nodeXPath
           pure $ DRoot n

root : Parser $ XPath ELEMENT ROOT
root = aRoot <|> dRoot

-- ------------------------------------------------------------------- [ Paths ]

data ParseRes = P (XPath ty PATH)
              | N (XPath ty NODE)
              | T (XPath ty (TEST b))

mutual
  pathelem : Parser $ ParseRes
  pathelem = do {res <- nodeTest; pure $ T (snd (snd res))}
         <|> do {res <- decpath; pure $ P (snd res)}
         <|> do {res <- anypath; pure $ P (snd res)}
         <|> map N nodeXPath

  thing : XPath ELEMENT ROOT -> ParseRes -> (ty ** XPath ty PATH)
  thing r (P p) = (_ ** r </> p)
  thing r (N n) = (_ ** r </> n)
  thing r (T t) = (_ ** r </> t)

  thing' : XPath ELEMENT NODE -> ParseRes -> (ty ** XPath ty PATH)
  thing' r (P p) = (_ ** r </> p)
  thing' r (N n) = (_ ** r </> n)
  thing' r (T t) = (_ ** r </> t)


  abspath : Parser $ (ty ** XPath ty PATH)
  abspath = do r <- root
               symbol "/"
               pelem <- pathelem
               pure (thing r pelem)

  anypath : Parser $ (ty ** XPath ty PATH)
  anypath = do r <- nodeXPath
               symbol "/"
               pelem <- pathelem
               pure (thing' r pelem)

  decpath : Parser $ (ty ** XPath ty PATH)
  decpath = do r <- nodeXPath
               symbol "/"
               symbol "/"

               pelem <- pathelem
               pure (thing' r pelem)

path : Parser $ (ty ** XPath ty PATH)
path = decpath <|> anypath <|> abspath

export
query : Parser $ (ty ** XPath ty QUERY)
query = do {res <- nodeTest;  pure (_ ** Query (snd (snd res)))}
    <|> do {res <- path;      pure (_** Query (snd res))}
    <|> do {res <- root;      pure (_ ** Query res)}
    <|> do {res <- nodeXPath; pure (_ ** Query res)}
{-
export
parseQuery : Parser $ (ty ** XPath ty QUERY)
parseQuery = do (_ ** _ ** res) <- nodetest; pure $ (_ ** Query res)
         <|> do (_ ** res) <- path; pure $ (_ ** Query res)
         <|> do res <- root; pure (_ ** Query res)
         <|> do res <- node; pure (_ ** Query res)
         <?> "XPath Query"
  -}


export
parseXPathStr : (doc : String)
             -> (Either (Run.ParseError Token) (ty ** XPath ty QUERY))
parseXPathStr = parseString XPathLexer query


export
parseXMLPathFile : (fname : String)
               -> IO (Either (Run.ParseError Token) (ty ** XPath ty QUERY))
parseXMLPathFile = parseFile XPathLexer query


-- --------------------------------------------------------------------- [ EOF ]
