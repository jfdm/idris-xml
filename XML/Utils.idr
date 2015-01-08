-- --------------------------------------------------------------- [ Utils.idr ]
-- Module      : XML.Utils
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- Utility functions for working with XML objects.
--
-- --------------------------------------------------------------------- [ EOH ]
module XML.Utils

import XML.Types

createXMLInfoDefault : XMLInfo
createXMLInfoDefault = MkXMLInfo "1.2" "UTF-8" True

createXMLInfo : String -> String -> Bool -> XMLInfo
createXMLInfo v e a = MkXMLInfo v e a


mkNode : (Document a) -> {auto prf : ValidNode a} -> Document NODES
mkNode n = [n]

mkNodeList : List (Document a) -> {auto prf : ValidNode a} -> Document NODES
mkNodeList Nil       = Types.Nil
mkNodeList (x :: xs) = Types.(::) x (mkNodeList xs)

(++) : Document NODES -> Document NODES -> Document NODES
(++) _       right = right
(++) (x::xs) right = x :: xs ++ right

delete : Document a
       -> {auto prf : ValidNode a}
       -> Document NODES
       -> Document NODES
delete _ Nil     = Nil
delete x (y::ys) = if eqDoc x y then ys else y :: delete x ys

isCons : Document NODES -> Bool
isCons Nil     = False
isCons (x::xs) = True

-- --------------------------------------------------------------------- [ EOF ]
