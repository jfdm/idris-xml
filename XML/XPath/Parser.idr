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
import public Lightyear.Strings
import XML.ParseUtils

%access private

nodetest : Parser $ XPath TEST
nodetest = do string "text()"; pure Text
  <|> do string "comment()"; pure Comment
  <|> do string "cdata()"; pure CData
  <|> do string "@"
         w <- word
         pure (Attr w)
  <?> "Node Tests"

node : Parser $ XPath NODE
node = do
    name <- map pack (some $ satisfy isAlphaNum)
    pure $ Elem name
  <?> "node"

root : Parser $ XPath ROOT
root = do
    string "/"
    name <- map pack (some $ satisfy isAlphaNum)
    pure $ Root name
  <?> "root"

droot : Parser $ XPath ROOT
droot = do
    string "//"
    name <- map pack (some $ satisfy isAlphaNum)
    pure $ DRoot name
  <?> "root"


data ParseRes = P (XPath PATH) | N (XPath NODE) | T (XPath TEST)

mutual
  pathelem : Parser $ ParseRes
  pathelem = map T nodetest
         <|> map P decpath
         <|> map P anypath
         <|> map N node
         <?> "Path Element"

  abspath : Parser $ XPath PATH
  abspath = do
      r <- root
      string "/"
      pelem <- pathelem
      case pelem of
        P p  => pure $ r </> p
        N n  => pure $ r </> n
        T t  => pure $ r </> t
    <?> "Absolute path"

  dabspath : Parser $ XPath PATH
  dabspath = do
      r <- droot
      string "/"
      pelem <- pathelem
      case pelem of
        P p  => pure $ r </> p
        N n  => pure $ r </> n
        T t  => pure $ r </> t
    <?> "Absolute path"


  anypath : Parser $ XPath PATH
  anypath = do
      r <- node
      string "/"
      pelem <- pathelem
      case pelem of
        P p  => pure $ r </> p
        N n  => pure $ r </> n
        T t  => pure $ r </> t
    <?> "Any Path"

  decpath : Parser $ XPath PATH
  decpath = do
      r <- node
      string "//" >! do
        pelem <- pathelem
        case pelem of
          P p  => pure $ r </> p
          N n  => pure $ r </> n
          T t  => pure $ r </> t
     <?> "Decendent Path"

path : Parser $ XPath PATH
path = dabspath <|> decpath <|> anypath <|> abspath <?> "Path"

public
parseQuery : Parser $ XPath QUERY
parseQuery = map Query nodetest
         <|> map Query path
         <|> map Query root
         <|> map Query node
         <?> "XPath Query"
-- --------------------------------------------------------------------- [ EOF ]
