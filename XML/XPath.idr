module XPath

import public XML.Types
import public XML.DOM

import public XML.XPath.Types
import XML.XPath.Parser

%access public

-- ------------------------------------------------------------------- [ Query ]

private
queryDoc : (a : NodeTy ** Document a) -> XPath a -> List $ Document ELEMENT
queryDoc n (Query q) = queryDoc n q
queryDoc n (Elem i)  = getElementsByName i n
queryDoc n (Root r)  = if getNodeName n == r then catMaybes [(getElement n)] else Nil
queryDoc n (Root p </> Elem c) = let es = getChildElementsByName p n in
    concatMap (\x => getChildElementsByName c (mkNode x)) es
queryDoc n (Root p </> c) with (n)
    | (ELEMENT ** e) = if getElementName e == p
                         then concatMap (\x => queryDoc x c) (getNodes e)
                         else Nil
    | otherwise = Nil
queryDoc n (Elem p </> Elem c) = let es = getElementsByName p n in
    concatMap (\x => getChildElementsByName c (mkNode x)) es
queryDoc n (Elem p </> c) = let es = getElementsByName p n in
    concatMap (\x => queryDoc (mkNode x) c) es
queryDoc n (p <//> c) = Nil
-- ------------------------------------------------------------------ [ Parser ]


query : String -> Document DOCUMENT -> Either String (List $ Document ELEMENT)
query qstr (MkDocument _ _ _ _ e) = case parse parseQuery qstr of
  Left err  => Left err
  Right res => case queryDoc (mkNode e) res of
    Nil => Left "Result not found"
    xs  => Right xs

query' : Document DOCUMENT -> String -> Either String (List $ Document ELEMENT)
query' d q = query q d


-- --------------------------------------------------------------------- [ EOF ]
