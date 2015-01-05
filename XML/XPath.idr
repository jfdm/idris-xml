module XPath

import public XML.Types
import public XML.DOM

import public XML.XPath.Types
import XML.XPath.Parser

%access public

-- ------------------------------------------------------------------- [ Query ]

private
queryDoc : Node -> XPath a -> List Element
queryDoc n (Query q) = queryDoc n q
queryDoc n (Elem i)  = getElementsByName i n
queryDoc n (Root r)  = case getElement n of
    Just n  => if getNodeName n == r then [n] else Nil
    Nothing => Nil
queryDoc n (Root p </> Elem c) = let es = getElementsByName p n in
    concatMap (\x => getChildElememtsByName c (NodeElement x)) es
queryDoc n (Root p </> c) = case getElement n of
    Just e => if getNodeName e == p
               then concatMap (\x => queryDoc x c) (getNodes e)
             else Nil
    Nothing => Nil
queryDoc n (Elem p </> Elem c) = let es = getElementsByName p n in
    concatMap (\x => getChildElememtsByName c (NodeElement x)) es
queryDoc n (Elem p </> c) = let es = getElementsByName p n in concatMap (\x => queryDoc (NodeElement x) c) es

queryDoc n (p <//> c) = Nil
-- ------------------------------------------------------------------ [ Parser ]


query : Document -> String -> Either String (List Element)
query doc qstr = case parse parseQuery qstr of
  Left err  => Left err
  Right res => case queryDoc (NodeElement (root doc)) res of
    Nil => Left "Result not found"
    xs  => Right xs

-- --------------------------------------------------------------------- [ EOF ]
