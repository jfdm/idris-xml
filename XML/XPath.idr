module XPath

import XML.DOM

import XML.XPath.Types
import XML.XPath.Parser
import public XML.XPath.Results

%access public

-- ------------------------------------------------------------------- [ Query ]

private
evaluatePath : XPath a -> Document ELEMENT -> XPathRes
evaluatePath (Query q) n = evaluatePath q n
evaluatePath (Elem e)  n = Elems $ getElementsByName e n
evaluatePath (Attr a)  n = case getAttribute a n of
    Just v => Vals [v]
    Nothing => Nout
evaluatePath (Text)    n = Prose $ (getText $ getNodes n)
evaluatePath (Comment) n = Docs $ getComments $ getNodes n
evaluatePath (Root r)  n = if getTagName n == r then Elems [n] else Nout
evaluatePath (DRoot r) n = Elems $ getElementsByName r n
-- evaluatePath (p </> c) n = concatMap (evaluatePath c) (evaluatePath p n)
evaluatePath (Root p </> branch) n with (branch)
    | Elem c = if getTagName n == p then Elems $ concatMap (getChildElementsByName c) (getChildElementsByName p n) else Nout
    | Text   = if getTagName n == p then Prose $ getText $ getNodes n else Nout
    | path   = if getTagName n == p then concatMap (evaluatePath path) (getChildElements n) else Nout
evaluatePath (Elem p </> branch) n with (branch)
    | Elem c = Elems $ concatMap (getChildElementsByName c) (getElementsByName p n)
    | Text   = Prose $ concatMap (\x => getText $ getNodes x) (getElementsByName p n)
    | path   = concatMap (evaluatePath path) (getElementsByName p n)
evaluatePath (Root p <//> branch) n with (branch)
    | Elem c = if getTagName n == p then Elems $ getElementsByName c n else Nout
    | path   = if getTagName n == p then concatMap (evaluatePath path) (getChildElements n) else Nout
evaluatePath (Elem p <//> branch) n with (branch)
    | Elem c = Elems $ concatMap (getElementsByName c) (getElementsByName p n)
    | path   = concatMap (evaluatePath path) (getElementsByName p n)

-- ------------------------------------------------------------------ [ Parser ]


query : String
      -> Document DOCUMENT
      -> Either String XPathRes
query qstr (MkDocument _ _ _ _ e) = case parse parseQuery qstr of
  Left err => Left err
  Right q  => Right $ evaluatePath q e

query' : Document DOCUMENT -> String -> Either String XPathRes
query' d q = query q d

-- --------------------------------------------------------------------- [ EOF ]
