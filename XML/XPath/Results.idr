module XPath.Results

import XML.DOM

data XPathRes = Elems (List $ Document ELEMENT)
              | Prose (List $ Document TEXT)
              | Docs  (List $ Document COMMENT)
              | Data  (List $ Document CDATA)
              | Vals  (List $ String)
              | Nout

instance Show x => Show XPathRes where
  show (Elems es) = show es
  show (Prose ps) = show ps
  show (Docs  ds) = show ds
  show (Data  ds) = show ds
  show (Vals  vs) = show vs
  show Nout       = "Nout"

instance Semigroup XPathRes where
  Nout <+> m = m
  m <+> Nout = m

  (Elems x) <+> m = m
  m <+> (Elems x) = m

  (Elems as) <+> (Elems bs) = Elems (as <+> bs)

  (Prose x) <+> m = m
  m <+> (Prose x) = m

  (Prose as) <+> (Prose bs) = Prose (as <+> bs)

  (Data x) <+> m = m
  m <+> (Data x) = m

  (Data as) <+> (Data bs) = Data (as <+> bs)

  (Docs x) <+> m = m
  m <+> (Docs x) = m

  (Docs as) <+> (Docs bs) = Docs (as <+> bs)

  (Vals x) <+> m = m
  m <+> (Vals x) = m

  (Vals as) <+> (Vals bs) = Vals (as <+> bs)


instance Monoid XPathRes where
  neutral = Nout

{-
resultCat : (a -> XPathRes) -> XPathRes -> XPathRes
resultCat _ Nout = Nout
resultCat f (Elems es) = Elems $ f <+> es
resultCat f (Prose ps) = Prose $ f <+> ps
resultCat f (Data ds)  = Data  $ f <+> ds
resultCat f (Docs ds)  = Docs  $ f <+> ds
resultCat f (Vals vs)  = Vals  $ f <+> vs
-}
-- --------------------------------------------------------------------- [ EOF ]
