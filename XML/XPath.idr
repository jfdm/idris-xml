module XML.XPath

import XML.DOM

import XML.XPath.Types
import XML.XPath.Parser

%access public

-- ------------------------------------------------------------------- [ ERROR ]

data XPathError : Type where
  MalformedQuery : String -> String -> XPathError

instance Show XPathError where
  show (MalformedQuery q err) =
    unlines [ unwords ["Query:", show q, "is malformed because"]
            , err]

-- ------------------------------------------------------------------- [ Query ]

private
evaluatePath : XPath a -> Document ELEMENT -> List $ Document NODE
evaluatePath (Query q) n = evaluatePath q n
evaluatePath (Elem e)  n = mkNodeList $ (getChildElementsByName e n)
evaluatePath (Any)     n = mkNodeList $ getChildElements n
evaluatePath (Attr a)  n = case getAttribute a n of
    Just v => [mkTextNode v]
    Nothing => Nil
evaluatePath (CData)   n = mkNodeList (getCData $ getNodes n)
evaluatePath (Text)    n = mkNodeList (getText $ getNodes n)
evaluatePath (Comment) n = mkNodeList (getComments $ getNodes n)
evaluatePath (Root r)  n with (r)
    | Any    = [Node n]
    | Elem e = if getTagName n == e then [Node n] else Nil
evaluatePath (DRoot r) n with (r)
    | Any    = mkNodeList $ getAllChildren n
    | Elem e = mkNodeList $ getElementsByName e n
evaluatePath (p </> c) n = concatMap (evaluatePath c) $ getElements (evaluatePath p n)
evaluatePath (p <//> child) n with (child)
    | Any    = mkNodeList $ concatMap (getAllChildren) $ getElements (evaluatePath p n)
    | Elem c = mkNodeList $ concatMap (getElementsByName c) $ getElements (evaluatePath p n)
    | path   = concatMap (evaluatePath path) $ getElements (evaluatePath p n)
-- ------------------------------------------------------------------ [ Parser ]

public
queryDoc : String
      -> Document DOCUMENT
      -> Either XPathError (List $ Document NODE)
queryDoc qstr (MkDocument _ _ _ _ e) =
  case parse parseQuery qstr of
    Left err => Left $ MalformedQuery qstr err
    Right q  => Right $ evaluatePath q e

public
queryDoc' : Document DOCUMENT -> String
          -> Either XPathError (List $ Document NODE)
queryDoc' d q = queryDoc q d

public
queryElem : String
         -> Document ELEMENT
         -> Either XPathError (List $ Document NODE)
queryElem qstr e = case parse parseQuery qstr of
  Left err => Left $ MalformedQuery qstr err
  Right q  => Right $ evaluatePath q e
-- --------------------------------------------------------------------- [ EOF ]
