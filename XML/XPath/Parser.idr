-- -------------------------------------------------------------- [ Parser.idr ]
-- Module      : XML.XPath.Parser
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- Turn an xpath dsl instance to edsl
-- --------------------------------------------------------------------- [ EOH ]
module XML.XPath.Parser

import XML.DOM

import XML.XPath.Types

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

import XML.ParseUtils

%access private

-- -------------------------------------------------------------------- [ Test ]
nodetest : Parser $ (b ** ty ** XPath ty (TEST b))
nodetest = do string "text()"; pure (_ ** _ ** Text)
  <|> do string "comment()"; pure (_ ** _ ** Comment)
  <|> do string "cdata()"; pure (_ ** _ ** CData)
  <|> do string "@"
         w <- word
         pure (_ ** _ ** Attr w)
  <?> "Node Tests"

-- ------------------------------------------------------------------- [ Nodes ]

strnode : Parser $ XPath ELEMENT NODE
strnode = do
    name <- map pack (some $ satisfy isAlphaNum)
    pure $ Elem name
  <?> "Named node"

anynode : Parser $ XPath ELEMENT NODE
anynode = do
    string "*"
    pure $ Any
  <?> "Wildcard"

node : Parser $ XPath ELEMENT NODE
node = anynode <|> strnode <?> "Nodes"

-- ------------------------------------------------------------------- [ Roots ]

aroot : Parser $ XPath ELEMENT ROOT
aroot = do
    string "/"
    n <- node
    pure $ Root n
  <?> "Absolute root"

droot : Parser $ XPath ELEMENT ROOT
droot = do
    string "//"
    n <- node
    pure $ DRoot n
  <?> "Descendent root"

root : Parser $ XPath ELEMENT ROOT
root = aroot <|> droot <?> "Root"

-- ------------------------------------------------------------------- [ Paths ]

data ParseRes = P (XPath ty PATH)
              | N (XPath ty NODE)
              | T (XPath ty (TEST b))

mutual
  pathelem : Parser $ ParseRes
  pathelem = do {(_ ** _ ** res) <- nodetest; pure $ T res}
         <|> do {(_ ** res) <- decpath; pure $ P res}
         <|> do {(_ ** res) <- anypath; pure $ P res}
         <|> map N node
         <?> "Path Element"

  abspath : Parser $ (ty ** XPath ty PATH)
  abspath = do
      r <- root
      string "/"
      pelem <- pathelem
      case pelem of
        P p  => pure $ (_ ** r </> p)
        N n  => pure $ (_ ** r </> n)
        T t  => pure $ (_ ** r </> t)
    <?> "Absolute path"

  anypath : Parser $ (ty ** XPath ty PATH)
  anypath = do
      r <- node
      string "/"
      pelem <- pathelem
      case pelem of
        P p  => pure $ (_ ** r </> p)
        N n  => pure $ (_ ** r </> n)
        T t  => pure $ (_ ** r </> t)
    <?> "Any Path"

  decpath : Parser $ (ty ** XPath ty PATH)
  decpath = do
      r <- node
      string "//" >! do
        pelem <- pathelem
        case pelem of
          P p  => pure $ (_ ** r </> p)
          N n  => pure $ (_ ** r </> n)
          T t  => pure $ (_ ** r </> t)
     <?> "Decendent Path"

path : Parser $ (ty ** XPath ty PATH)
path = decpath <|> anypath <|> abspath <?> "Path"

export
parseQuery : Parser $ (ty ** XPath ty QUERY)
parseQuery = do (_ ** _ ** res) <- nodetest; pure $ (_ ** Query res)
         <|> do (_ ** res) <- path; pure $ (_ ** Query res)
         <|> do res <- root; pure (_ ** Query res)
         <|> do res <- node; pure (_ ** Query res)
         <?> "XPath Query"
-- --------------------------------------------------------------------- [ EOF ]
