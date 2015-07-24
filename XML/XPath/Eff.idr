||| Effectful helper operations
module XML.XPath.Eff

import Effects
import Effect.Exception

import XML.DOM
import XML.XPath

queryDocE : String
         -> Document DOCUMENT
         -> {[EXCEPTION String]} Eff (List $ Document NODE)
queryDocE q d = case queryDoc q d of
  Left err  => raise err
  Right res => pure res

queryElemE : String
          -> Document ELEMENT
          -> {[EXCEPTION String]} Eff (List $ Document NODE)
queryElemE q e = case queryElem q e of
   Left err  => raise err
   Right res => pure res
-- --------------------------------------------------------------------- [ EOF ]
