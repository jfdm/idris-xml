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
import XML.Utils
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

instruction : Parser $ Document INSTRUCTION
instruction = token "<?" >! do
    itarget <- word <$ space
    idata  <- some $ genKVPair (word <$ space) (dquote url)
    token "?>"
    pure $ Instruction itarget idata
  <?> "Instruction"

text : Parser $ (Document TEXT)
text = do
    txt <- some (xmlWord <$ space)
    pure $ Text $ unwords txt
  <?> "Text Node"

cdata : Parser $ (Document CDATA)
cdata = do
    token "<![CDATA[" >! do
      txt <- manyTill (anyChar) (token "]]>")
      pure $ CData $ pack txt
  <?> "CData"

empty : Parser $ (Document ELEMENT)
empty = do
    (_, qn, as) <- elemStart <$ space
    token "/" >! do
      token ">"
      pure $ Element qn as Nil
  <?> "Empty Node"

mutual

  public
  node : Parser $ Document NODES
  node = map (\x => mkNode x) comment
     <|> map (\x => mkNode x) cdata
     <|> map (\x => mkNode x) instruction
     <|> map (\x => mkNode x) empty
     <|> map (\x => mkNode x) element
     <|> map (\x => mkNode x) text
     <?> "Nodes"

  element : Parser $ Document ELEMENT
  element = do
      (n, qn, as) <- elemStart <$ space
      token ">" $!> do
        cs <- some node
        elemEnd $ trim n
        pure $ Element qn as $ foldr XML.Utils.(++) XML.Types.Nil cs
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
    root <- element -- Add check if docttype exists for name of root element
    pure $ MkDocument info dtype is doc root
  <?> "XML DOcument"
-- --------------------------------------------------------------------- [ EOF ]
