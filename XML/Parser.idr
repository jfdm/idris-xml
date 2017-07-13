-- -------------------------------------------------------------- [ Parser.idr ]
-- Module      : XML.Parser
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- A DOM-based XML parser.
-- --------------------------------------------------------------------- [ EOH ]
module XML.Parser

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

import XML.DOM
import XML.ParseUtils

%access private

-- ------------------------------------------------------------------- [ Utils ]
qname : Parser (String, Document QNAME)
qname = do
    pre <- opt $ (word <* colon)
    name <- word
    spaces
    let pr = case pre of
        Just p => p ++ ":"
        Nothing => ""
    let tag = pr ++ name
    pure (tag, mkQName name Nothing pre)

attr : Parser $ (Document QNAME, String)
attr = do
    ((_,qn),v) <- genKVPair (qname) (quoted '\"')
    spaces
    pure (qn, v)
  <?> "Node Attributes"

elemStart : Parser (String, Document QNAME, (List (Document QNAME, String)))
elemStart = do
    token "<"
    (tag, qn) <- qname
    ns <- opt $ keyvalue' "xmlns" (quoted '\"')
    as <- opt $ some (attr)
    pure (tag, setNameSpace ns qn, fromMaybe Nil as)
  <?> "Start Tag"

elemEnd : String -> Parser ()
elemEnd s = angles (token "/" *!> token s) <?> "End Tag"

-- ------------------------------------------------------------------- [ Nodes ]
comment : Parser $ Document COMMENT
comment = token ("<!--") >! do
    cs <- map pack $ manyTill (anyChar) (spaces *> token "-->")
    pure $ mkComment cs
  <?> "Comment"

instruction : Parser $ Document INSTRUCTION
instruction = token "<?" >! do
    itarget <- word <* spaces
    idata  <- some $ genKVPair (word <* spaces) (dquote url)
    token "?>"
    pure $ mkInstruction itarget idata
  <?> "Instruction"

text : Parser $ (Document TEXT)
text = do
    txt <- some (xmlWord <* spaces)
    pure $ mkText( unwords txt)
  <?> "Text Node"

cdata : Parser $ (Document CDATA)
cdata = do
    token "<![CDATA[" >! do
      txt <- manyTill (anyChar) (token "]]>")
      pure $ mkCData $ pack txt
  <?> "CData"

empty : Parser $ (Document ELEMENT)
empty = do
    (_, qn, as) <- elemStart <* spaces
    token "/>" >! do
      pure $ mkElement qn as Nil
  <?> "Empty Node"

private
buildNodeList : List (ty ** prf : ValidNode ty ** Document ty) -> (ts ** prfs ** NodeList ts prfs)
buildNodeList [] = ([] ** [] ** [])
buildNodeList ((ty ** prf ** node) :: xs) =
    let (ts ** prfs ** nodes) = buildNodeList xs
     in (ty :: ts ** prf :: prfs ** node :: nodes)

mutual

  export
  node : Parser $ (ty ** prf : ValidNode ty ** Document ty)
  node = map (\n => (COMMENT     ** ValidDoc   ** n)) comment
     <|> map (\n => (CDATA       ** ValidCData ** n)) cdata
     <|> map (\n => (INSTRUCTION ** ValidInstr ** n)) instruction
     <|> map (\n => (ELEMENT     ** ValidElem  ** n)) empty
     <|> map (\n => (ELEMENT     ** ValidElem  ** n)) element
     <|> map (\n => (TEXT        ** ValidText  ** n)) text
     <?> "Nodes"

  element : Parser $ Document ELEMENT
  element = do
      (n, qn, as) <- elemStart <* spaces
      token ">" *!> do
        ns <- some node
        elemEnd $ trim n
        let (_ ** _ ** nodes) = buildNodeList ns
        pure $ mkElement qn as nodes
     <?> "Element"

isStandalone : Parser Bool
isStandalone = (expValue "yes" *> pure True)
           <|> (expValue "true" *> pure True) <?> "Expected Standalone"

notStandalone : Parser Bool
notStandalone = (expValue "no" *> pure False)
           <|> (expValue "false" *> pure False) <?> "Expected not Standalone"

export
xmlinfo : Parser $ Document INFO
xmlinfo = token "<?xml" *!> do
    vers <- keyvalue' "version" $ quoted '\"' <* spaces
    enc  <- opt $ keyvalue' "encoding" $ quoted '\"' <* spaces
    alone <- opt $ keyvalue' "standalone" standalone <* spaces
    token "?>"
    pure $ mkXMLInfo vers (fromMaybe "UTF-8" enc) (fromMaybe True alone)
   <?> "XML Info"
  where
    standalone : Parser Bool
    standalone = isStandalone <|> notStandalone <?> "Standalone"

pubident : Parser $ Document IDENT
pubident = do
    token "PUBLIC" >! do
      loc  <- quoted '\"' <* spaces
      loc' <- quoted '\"' <* spaces
      pure $ mkPublicID loc loc'
  <?> "Public identifer"

sysident : Parser $ Document IDENT
sysident = do
    token "SYSTEM" >! do
      loc <- quoted '\"' <* spaces
      pure $ mkSystemID loc
  <?> "System Identifier"

ident : Parser $ Document IDENT
ident = pubident <|> sysident <?> "Identifiers"

||| Parse Doctypes
export
doctype : Parser $ Document DOCTYPE
doctype = angles body <?> "DocType"
  where
    body : Parser $ Document DOCTYPE
    body = do
        token "!DOCTYPE" >! do
          v <- word <* spaces
          id <- opt ident <* spaces
          pure $ mkDocType v id
      <?> "DocType Body"

export
parseXMLSnippet : Parser $ Document ELEMENT
parseXMLSnippet = element <?> "XML Element"

export
parseXMLDoc : Parser $ Document DOCUMENT
parseXMLDoc = do
    info  <- xmlinfo <* spaces
    dtype <- opt doctype <* spaces
    is    <- many instruction <* spaces
    doc   <- opt comment <* spaces
    root  <- element -- Add check if docttype exists for name of root element
    pure $ mkDocument info dtype is doc root
  <?> "XML DOcument"
-- --------------------------------------------------------------------- [ EOF ]
