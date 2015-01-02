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

genKVPair : Parser a -> Parser b -> Parser (a, b)
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
    (_,v) <- genKVPair (token key) value
    pure v

comment : Parser String
comment = token ("<!--") >! do
    cs <- map pack $ manyTill (anyChar) (space $> token "-->")
    pure $ cs

instruction : Parser Instruction
instruction = token "<?" >! do
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

attr : Parser $ (QName, String)
attr = do
  (k,v) <- genKVPair (word <$ space) (literallyBetween '"')
  space
  pure (MkQName k Nothing Nothing, v)

cData : Parser Node
cData = do
  token "<![CDATA[" >! do
    txt <- manyTill (anyChar) (token "]]>")
    pure $ NodeContent $ pack txt

elemStart : Parser (String, QName, Maybe (List (QName, String)))
elemStart = do
  token "<"
  n <- word <$ space
  as <- opt $ some (attr)
  pure (n, MkQName n Nothing Nothing, as)

elemEnd : String -> Parser ()
elemEnd s = angles (token "/" $> string s)

emptyNode : Parser Node
emptyNode = do
  (_, qn, as) <- elemStart <$ space
  token "/" >! do
    token ">"
    pure $ NodeElement $ MkElement qn as Nil

mutual
  node : Parser Node
  node = iNode <|> cNode <|> cData <|> emptyNode <|> eNode <|> tNode

  eNode : Parser Node
  eNode = map NodeElement element

  element : Parser Element
  element = do
    (n, qn, as) <- elemStart <$ space
    token ">" >! do
      cs <- some tNode <$ (elemEnd n)
      pure $ MkElement qn as cs

mnode : Parser MetaNode
mnode = map MetaInstruction instruction <|> map MetaComment comment

xmlnode : Parser XMLNode
xmlnode = token "<?xml" $!> do
    vers <- keyvalue' "version" $ literallyBetween '\"'
    enc  <- opt $ keyvalue' "encoding" $ literallyBetween '\"'
    alone <- opt $ keyvalue' "standalone" standalone
    token "?>"
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
  -- Add check if docttype exists for name of root element
  doc <- element
  epilogue <- many mnode
  let prologue = MkPrologue xnode prologue_before dtype prologue_after
  pure $ MkDoc prologue doc epilogue

-- --------------------------------------------------------------------- [ EOF ]
