module XPath

import XML.DOM

import XML.XPath.Types
import XML.XPath.Parser

%access public

-- ------------------------------------------------------------------- [ Query ]

private
queryDoc : XPath a -> Document ELEMENT -> List $ Document ELEMENT
queryDoc (Query q) n = queryDoc q n
queryDoc (Elem i)  n = getElementsByName i n
queryDoc (Root r)  n = if getTagName n == r then [n] else Nil
queryDoc (Root p </> Elem c) n = let es = getChildElementsByName p n in
    concatMap (getChildElementsByName c) es
queryDoc (Root p </> c) n = if getTagName n == p
    then concatMap (queryDoc c) (getChildElements n)
    else Nil

queryDoc (Elem p </> Elem c) n = let es = getElementsByName p n in
    concatMap (getChildElementsByName c) es
queryDoc (Elem p </> c) n = let es = getElementsByName p n in
    concatMap (queryDoc c) es
queryDoc (p <//> c) n = Nil
-- ------------------------------------------------------------------ [ Parser ]


query : String
      -> Document DOCUMENT
      -> Either String (List $ Document ELEMENT)
query qstr (MkDocument _ _ _ _ e) = case parse parseQuery qstr of
  Left err => Left err
  Right q  => Right $ queryDoc q e

query' : Document DOCUMENT -> String -> Either String (List $ Document ELEMENT)
query' d q = query q d

-- --------------------------------------------------------------------- [ EOF ]
