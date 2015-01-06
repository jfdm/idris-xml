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

isStandalone : Parser Bool
isStandalone = (expValue "yes" $> return True)
           <|> (expValue "true" $> return True) <?> "Expected Standalone"

notStandalone : Parser Bool
notStandalone = (expValue "no" $> return False)
           <|> (expValue "false" $> return False) <?> "Expected not Standalone"

public
xmlinfo : Parser XMLInfo
xmlinfo = token "<?xml" $!> do
    vers <- keyvalue' "version" $ literallyBetween '\"' <$ space
    enc  <- opt $ keyvalue' "encoding" $ literallyBetween '\"' <$ space
    alone <- opt $ keyvalue' "standalone" standalone <$ space
    token "?>"
    pure $ MkXMLInfo vers (fromMaybe "UTF-8" enc) (fromMaybe True alone)
   <?> "XML Info"
  where
    standalone : Parser Bool
    standalone = isStandalone <|> notStandalone <?> "Standalone"

pubident : Parser ExternalID
pubident = do
    token "PUBLIC" >! do
      loc <- literallyBetween '\"' <$ space
      loc' <- literallyBetween '\"' <$ space
      pure $ PublicID loc loc'
  <?> "Public identifer"

sysident : Parser ExternalID
sysident = do
    token "SYSTEM" >! do
      loc <- literallyBetween '\"' <$ space
      pure $ SystemID loc
  <?> "System Identifier"

ident : Parser ExternalID
ident = pubident <|> sysident <?> "Identifiers"

||| Parse Doctypes
public
doctype : Parser DocType
doctype = angles body <?> "DocType"
  where
    body : Parser $ DocType
    body = do
        token "!DOCTYPE" >! do
          v <- word <$ space
          id <- opt ident <$ space
          pure $ MkDocType v id
      <?> "DocType Body"

public
parseXML : Parser $ Document DOCUMENT
parseXML = do
    info <- xmlinfo <$ space
    dtype <- opt doctype <$ space
    is <- many instruction <$ space
    doc <- opt comment <$ space
    root <- rootElem -- Add check if docttype exists for name of root element
    pure $ MkDocument info dtype is doc root
  <?> "XML DOcument"
-- --------------------------------------------------------------------- [ EOF ]
