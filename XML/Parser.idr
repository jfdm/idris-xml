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
-- rewrite to parse left angle do stuff, otherwise it is content.
%access public

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

doctype : Parser DocType
doctype = do
  v <- angles (token "!DOCTYPE" $> word)
  pure $ MkDocType v Nothing

public
parseXML : Parser Document
parseXML = do
  prologue_before <- many mnode
  dtype <- opt doctype
  prologue_after <- many mnode
  doc <- element
  epilogue <- many mnode
  let prologue = MkPrologue prologue_before dtype prologue_after
  pure $ MkDoc prologue doc epilogue

-- --------------------------------------------------------------------- [ EOF ]
