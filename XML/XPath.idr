module XPath

import XML.DOM

import XML.XPath.Types
import XML.XPath.Parser

%access public

-- ------------------------------------------------------------------- [ Query ]

private
evaluatePath : XPath a -> Document ELEMENT -> Document NODES
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
    | Any    = [n]
    | Elem e = if getTagName n == e then [n] else Nil
evaluatePath (DRoot r) n with (r)
    | Any    = mkNodeList $ getAllChildren n
    | Elem e = mkNodeList $ getElementsByName e n
evaluatePath (p </> c) n = concatMap (evaluatePath c) $ getElements (evaluatePath p n)
evaluatePath (p <//> child) n with (child)
    | Any    = mkNodeList $ concatMap (getAllChildren) $ getElements (evaluatePath p n)
    | Elem c = mkNodeList $ concatMap (getElementsByName c) $ getElements (evaluatePath p n)
    | path   = concatMap (evaluatePath path) $ getElements (evaluatePath p n)
-- ------------------------------------------------------------------ [ Parser ]


query : String
      -> Document DOCUMENT
      -> Either String (Document NODES)
query qstr (MkDocument _ _ _ _ e) = case parse parseQuery qstr of
  Left err => Left err
  Right q  => Right $ evaluatePath q e

query' : Document DOCUMENT -> String -> Either String (Document NODES)
query' d q = query q d

-- --------------------------------------------------------------------- [ EOF ]
