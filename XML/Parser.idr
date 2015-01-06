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

%access private

-- ------------------------------------------------------------------- [ Utils ]

attr : Parser $ (QName, String)
attr = do
    (k,v) <- genKVPair (word <$ space) (dquote url)
    space
    pure (MkQName k Nothing Nothing, v)
  <?> "Node Attributes"

elemStart : Parser (String, QName, (List (QName, String)))
elemStart = do
    token "<"
    n <- word <$ space
    as <- opt $ some (attr)
    pure (n, MkQName n Nothing Nothing, fromMaybe Nil as)
  <?> "Start Tag"

elemEnd : String -> Parser ()
elemEnd s = angles (token "/" $!> token s) <?> "End Tag"

-- ------------------------------------------------------------------- [ Nodes ]
comment : Parser $ Document COMMENT
comment = token ("<!--") >! do
    cs <- map pack $ manyTill (anyChar) (space $> token "-->")
    pure $ Comment cs
  <?> "Comment"

commentNode : Parser $ (COMMENT ** Document COMMENT)
commentNode = do
    c <- comment
    pure $ mkNode c
  <?> "Comment Node"

instruction : Parser $ Document INSTRUCTION
instruction = token "<?" >! do
    itarget <- word <$ space
    idata  <- some $ genKVPair (word <$ space) (dquote url)
    token "?>"
    pure $ Instruction itarget idata
  <?> "Instruction"

instructionNode : Parser $ (COMMENT ** Document COMMENT)
instructionNode = do
    c <- instruction
    pure $ mkNode c
  <?> "Comment Node"

text : Parser $ (TEXT ** Document TEXT)
text = do
    txt <- some (xmlWord <$ space)
    pure $ mkNode $ Text $ unwords txt
  <?> "Text Node"

cdata : Parser $ (CDATA ** Document CDATA)
cdata = do
    token "<![CDATA[" >! do
      txt <- manyTill (anyChar) (token "]]>")
      pure $ mkNode $ CData $ pack txt
  <?> "CData"

empty : Parser $ (ELEMENT ** Document ELEMENT)
empty = do
    (_, qn, as) <- elemStart <$ space
    token "/" >! do
      token ">"
      pure $ mkNode $ Element qn as Nil
  <?> "Empty Node"

mutual
  public
  node : Parser $ (a : NodeTy ** Document a)
  node = commentNode
     <|> cdata
     <|> instructionNode
     <|> empty
     <|> element
     <|> text
     <?> "Nodes"

  element : Parser $ (ELEMENT ** Document ELEMENT)
  element = do
      e <- rootElem
      pure $ mkNode e
    <?> "Element Node"

  rootElem : Parser $ Document ELEMENT
  rootElem = do
      (n, qn, as) <- elemStart <$ space
      token ">" $!> do
        cs <- some node
        elemEnd $ trim n
        pure $ Element qn as cs
     <?> "Element"

public
xmlinfo : Parser XMLInfo
xmlinfo = token "<?xml" $!> do
    vers <- keyvalue' "version" $ literallyBetween '\"'
    enc  <- opt $ keyvalue' "encoding" $ literallyBetween '\"'
    alone <- opt $ keyvalue' "standalone" standalone
    token "?>"
    pure $ MkXMLInfo vers (fromMaybe "UTF-8" enc) (fromMaybe True alone)
  where
    standalone : Parser Bool
    standalone = do
           skip $ dquote (string "yes")
           pure True
        <|> do
           skip $ dquote (string "no")
           pure False

public
doctype : Parser DocType
doctype = do
  v <- angles (token "!DOCTYPE" $> word)
  pure $ MkDocType v Nothing

public
parseXML : Parser $ Document DOCUMENT
parseXML = do
  info <- xmlinfo
  dtype <- opt doctype
  is <- many instruction
  doc <- opt comment
  root <- rootElem -- Add check if docttype exists for name of root element
  pure $ MkDocument info dtype is doc root

-- --------------------------------------------------------------------- [ EOF ]
