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

comment : Parser String
comment = token ("<!--") >! do
    cs <- map pack $ manyTill (anyChar) (space $> token "-->")
    pure $ cs
  <?> "Comment"

instruction : Parser Instruction
instruction = token "<?" >! do
    itarget <- word <$ space
    idata  <- some $ genKVPair (word <$ space) (dquote url)
    token "?>"
    pure $ MkInstruction itarget idata
  <?> "Instruction"

cNode : Parser $ Document COMMENT
cNode = map Comment comment <?> "Comment Node"

tNode : Parser $ Document TEXT
tNode = do
    txt <- some (xmlWord <$ space)
    pure $ Text $ unwords txt
  <?> "Text Node"

attr : Parser $ (QName, String)
attr = do
    (k,v) <- genKVPair (word <$ space) (dquote url)
    space
    pure (MkQName k Nothing Nothing, v)
  <?> "Node Attributes"

cData : Parser $ Document CDATA
cData = do
    token "<![CDATA[" >! do
      txt <- manyTill (anyChar) (token "]]>")
      pure $ CData $ pack txt
  <?> "CData"

elemStart : Parser (String, QName, (List (QName, String)))
elemStart = do
    token "<"
    n <- word <$ space
    as <- opt $ some (attr)
    pure (n, MkQName n Nothing Nothing, fromMaybe Nil as)
  <?> "Start Tag"

elemEnd : String -> Parser ()
elemEnd s = angles (token "/" $!> token s) <?> "End Tag"

emptyNode : Parser $ Document ELEMENT
emptyNode = do
    (_, qn, as) <- elemStart <$ space
    token "/" >! do
      token ">"
      pure $ Element qn as Nil
  <?> "Empty Node"

mutual
  public
  node : {a : NodeTy} -> Parser $ Document ?what
  node = cNode <|> cData <|> emptyNode <|> element <|> tNode <?> "Nodes"

  element : Parser $ Document ELEMENT
  element = do
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
  doc <- map Comment comment
  root <- element -- Add check if docttype exists for name of root element
  pure $ MkDocument info dtype is doc root

-- --------------------------------------------------------------------- [ EOF ]
