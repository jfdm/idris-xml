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
evaluatePath (Attr a)  n = case getAttribute a n of
    Just v => [mkTextNode v]
    Nothing => Nil
evaluatePath (CData)   n = mkNodeList (getCData $ getNodes n)
evaluatePath (Text)    n = mkNodeList (getText $ getNodes n)
evaluatePath (Comment) n = mkNodeList (getComments $ getNodes n)
evaluatePath (Root r)  n = if getTagName n == r then [n] else Nil
evaluatePath (DRoot r) n = mkNodeList $ getElementsByName r n
evaluatePath (p </> c) n = concatMap (evaluatePath c) $ getElements (evaluatePath p n)
evaluatePath (Root p <//> branch) n with (branch)
    | Elem c = if getTagName n == p then mkNodeList $ getElementsByName c n else Nil
    | path   = if getTagName n == p then concatMap (evaluatePath path) (getChildElements n) else Nil
evaluatePath (Elem p <//> branch) n with (branch)
    | Elem c = mkNodeList $ concatMap (getElementsByName c) (getElementsByName p n)
    | path   = concatMap (evaluatePath path) (getElementsByName p n)
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
