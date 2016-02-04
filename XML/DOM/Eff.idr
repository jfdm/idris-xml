||| Convenience functions for working in Effectful functions.
module XML.DOM.Eff

import Effects
import Effect.Exception

import XML.DOM

%access export

getAttributeE : String
             -> Document ELEMENT
             -> {[EXCEPTION String]} Eff String
getAttributeE id n = case getAttribute id n of
  Just res => pure res
  Nothing  => raise $ "Element does not have: " ++ id

-- --------------------------------------------------------------------- [ EOF ]
