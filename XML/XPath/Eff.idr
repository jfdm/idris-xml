||| Effectful helper operations
module XML.XPath.Eff

import Effects
import Effect.Exception

import XML.XPath

xQuery : String
       -> Document DOCUMENT
       -> {[EXCEPTION String]} Eff (List (Document NODE))
xQuery q d = case query q d of
  Left err  => raise err
  Right res => pure res

-- --------------------------------------------------------------------- [ EOF ]
