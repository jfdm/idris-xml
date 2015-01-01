-- -------------------------------------------------------------- [ Parser.idr ]
-- Module      : XML.Parser
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- A DOM-based XML parser.
-- --------------------------------------------------------------------- [ EOH ]
module XML.Parser

import Lightyear
import Lightyear.Strings

import XML.Types
import XML.ParseUtils

%access public

genKVPair : Parser String -> Parser a -> Parser (String, a)
genKVPair key value = do
    k <- key
    equals
    v <- value <$ space
    pure (k,v)
  <?> "KVPair"

keyvalue : Parser (String, String)
keyvalue = do
    (k,v) <- genKVPair (word <$ space) (literallyBetween '\"')
    pure (k, v)
  <?> "KVPair"

keyvalue' : String -> Parser a -> Parser a
keyvalue' key value = do
    token key
    equals
    v <- value <$ space
    pure v

comment : Parser String
comment = token ("<!--") $!> do
    cs <- map pack $ manyTill (anyChar) (token "-->")
    pure $ cs

instruction : Parser Instruction
instruction = token "<?" $!> do
    itarget <- word <$ space
    idata  <- map pack $ manyTill (anyChar) (token "?>")
    pure $ MkInstruction itarget idata

iNode : Parser Node
iNode = map NodeInstruction instruction

cNode : Parser Node
cNode = map NodeComment comment

tNode : Parser Node
tNode = do
  txt <- some (anyChar <$ space)
  pure $ NodeContent $ pack txt

mutual
  node : Parser Node
  node = iNode <|> cNode <|> map NodeElement element
       <|> tNode

  element : Parser Element
  element = do
    n <- char '<' $> word
    token ">" $!> do
      children <- some node
      angles (char '/' $> string n)
      pure $ MkElement (MkQName n Nothing Nothing) Nothing children

mnode : Parser MetaNode
mnode = map MetaInstruction instruction <|> map MetaComment comment

xmlnode : Parser XMLNode
xmlnode = token "<?xml" $!> do
    vers <- keyvalue' "version" $ literallyBetween '\"'
    enc  <- opt $ keyvalue' "encoding" $ literallyBetween '\"'
    alone <- opt $ keyvalue' "standalone" standalone
    pure $ MkXMLNode vers (fromMaybe "UTF-8" enc) (fromMaybe True alone)
  where
    standalone : Parser Bool
    standalone = do
           skip $ dquote (string "yes")
           pure True
        <|> do
           skip $ dquote (string "no")
           pure False

doctype : Parser DocType
doctype = do
  v <- angles (token "!DOCTYPE" $> word)
  pure $ MkDocType v Nothing

public
parseXML : Parser Document
parseXML = do
  xnode <- xmlnode
  prologue_before <- many mnode
  dtype <- opt doctype
  prologue_after <- many mnode
  doc <- element
  epilogue <- many mnode
  let prologue = MkPrologue xnode prologue_before dtype prologue_after
  pure $ MkDoc prologue doc epilogue

-- --------------------------------------------------------------------- [ EOF ]
