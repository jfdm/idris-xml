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

%access private

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

mutual
  pathelem : Parser $ Either (XPath PATH) (XPath NODE)
  pathelem = map Left decpath <|> map Left anypath <|> map Right node <?> "Path Element"

  abspath : Parser $ XPath PATH
  abspath = do
      r <- root
      string "/"
      pelem <- pathelem
      case pelem of
        Left p  => pure $ r </> p
        Right n => pure $ r </> n
    <?> "Absolute path"

  anypath : Parser $ XPath PATH
  anypath = do
      r <- node
      string "/"
      pelem <- pathelem
      case pelem of
        Left p  => pure $ r </> p
        Right n => pure $ r </> n
    <?> "Any Path"

  decpath : Parser $ XPath PATH
  decpath = do
      r <- node
      string "//" >! do
        pelem <- pathelem
        case pelem of
          Left p  => pure $ r <//> p
          Right n => pure $ r <//> n
     <?> "Decendent Path"

path : Parser $ XPath PATH
path = decpath <|> anypath <|> abspath <?> "Path"

public
parseQuery : Parser $ XPath QUERY
parseQuery = map Query path <|> map Query root <|> map Query node

-- --------------------------------------------------------------------- [ EOF ]
