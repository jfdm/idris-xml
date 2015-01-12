module XPath

import XML.DOM

import XML.XPath.Types
import XML.XPath.Parser

%access public

-- ------------------------------------------------------------------- [ Query ]

data QueryRes = Elems (List $ Document ELEMENT)
              | Prose (List $ Document TEXT)
              | Docs  (List $ Document COMMENT)
              | Values (List String)
              | Any (Document NODES)
              | Nout

instance Show QueryRes where
  show (Elems es) = show es
  show (Prose ps) = show ps
  show (Docs ds)  = show ds
  show (Any as)   = show as
  show (Values vs) = show vs
  show (Nout)      = "Nout"


instance Semigroup QueryRes where
  Nout <+> m = m
  m <+> Nout = m
  (Elems x) <+> m = m
  m <+> (Elems x) = m

  (Prose x) <+> m = m
  m <+> (Prose x) = m

  (Docs x) <+> m = m
  m <+> (Docs x) = m

  (Values x) <+> m = m
  m <+> (Values x) = m

  (Any x) <+> m = m
  m <+> (Any x) = m


  (Any as)   <+>  (Any bs)  = Any (as ++ bs)
  (Elems as) <+> (Elems bs) = Elems (as <+> bs)
  (Prose as) <+> (Prose bs) = Prose (as <+> bs)
  (Docs as) <+> (Docs bs)   = Docs (as <+> bs)
  (Values as) <+> (Values bs) = Values (as <+> bs)

instance Monoid QueryRes where
  neutral = Nout


private
queryDoc : XPath a -> Document ELEMENT -> QueryRes
queryDoc (Query q) n = queryDoc q n
queryDoc (Elem i)  n = Elems $ getElementsByName i n
queryDoc (Attr a)  n = case getAttribute a n of
    Just v => Values [v]
    Nothing => Nout
queryDoc (Text)    n = Nout
queryDoc (Comment) n = Nout
queryDoc (Root r)  n = if getTagName n == r then Elems [n] else Nout
queryDoc (Root p </> Elem c) n = let es = getChildElementsByName p n in
    Elems $ concatMap (getChildElementsByName c) es
queryDoc (Root p </> c) n = if getTagName n == p
    then concatMap (queryDoc c) (getChildElements n)
    else Nout

queryDoc (Elem p </> Elem c) n = let es = getElementsByName p n in
    Elems $ concatMap (getChildElementsByName c) es
queryDoc (Elem p </> c) n = let es = getElementsByName p n in
    concatMap (queryDoc c) es
queryDoc (p <//> c) n = Nout
-- ------------------------------------------------------------------ [ Parser ]


query : String
      -> Document DOCUMENT
      -> Either String QueryRes
query qstr (MkDocument _ _ _ _ e) = case parse parseQuery qstr of
  Left err => Left err
  Right q  => Right $ queryDoc q e

query' : Document DOCUMENT -> String -> Either String QueryRes
query' d q = query q d

-- --------------------------------------------------------------------- [ EOF ]
