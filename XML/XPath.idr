module XPath

import public XML.Types
import public XML.DOM

import public XML.XPath.Types
import XML.XPath.Parser

%access public

-- ------------------------------------------------------------------- [ Query ]

private
queryDoc : Node -> XPath a -> List Element
queryDoc e (Query q) = queryDoc e q
queryDoc e (Elem i)  = getElementsByName i e
queryDoc e (Root r)  = case getElement e of
    Just n  => if getNodeName e == r then [n] else Nil
    Nothing => Nil
queryDoc e (Root p </> c) = case getElement e of
    Just n => if getNodeName n == p
               then concatMap (\x => queryDoc x c) (getNodes e)
             else Nil
    Nothing => Nil
queryDoc e (Elem p </> c) = let es = getElementsByName p e in concatMap (\x => queryDoc (NodeElement x) c) es

-- ------------------------------------------------------------------ [ Parser ]


query : Document -> String -> Either String (List Element)
query doc qstr = case parse parseQuery qstr of
  Left err  => Left err
  Right res => case queryDoc (NodeElement (root doc)) res of
    Nil => Left "Result not found"
    xs  => Right xs

-- --------------------------------------------------------------------- [ EOF ]
