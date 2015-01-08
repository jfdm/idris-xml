-- --------------------------------------------------------------- [ Utils.idr ]
-- Module      : XML.Utils
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- Utility functions for working with XML objects.
--
-- --------------------------------------------------------------------- [ EOH ]
module XML.DOM.Utils

import XML.DOM.Model
import XML.DOM.Eq

setRoot : Document ELEMENT -> Document a -> Document DOCUMENT
setRoot newe (MkDocument info dtype ins doc e) = MkDocument info dtype ins doc newe

createXMLInfoDefault : XMLInfo
createXMLInfoDefault = MkXMLInfo "1.2" "UTF-8" True

createXMLInfo : String -> String -> Bool -> XMLInfo
createXMLInfo v e a = MkXMLInfo v e a


mkNode : (Document a) -> {default IsOK  p : ValidNode a}-> Document NODES
mkNode n = [n]

mkNodeList : List (Document a)
           -> {default IsOK p : ValidNode a}
           -> Document NODES
mkNodeList Nil       = DOM.Model.Nil
mkNodeList (x :: xs) = DOM.Model.(::) x (mkNodeList xs)

(++) : Document NODES -> Document NODES -> Document NODES
(++) _       right = right
(++) (x::xs) right = x :: xs ++ right

delete : Document a
       -> {default IsOK p : ValidNode a}
       -> Document NODES
       -> Document NODES
delete _ Nil     = Nil
delete x (y::ys) = if eqDoc x y then ys else y :: delete x ys

isCons : Document NODES -> Bool
isCons Nil     = False
isCons (x::xs) = True

-- --------------------------------------------------------------------- [ EOF ]
