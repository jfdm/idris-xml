-- ----------------------------------------------------------------- [ Eff.idr ]
-- Module    : Eff.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

||| XPath but in an effectfull context.
module XML.XPath.Eff

import Effects
import Effect.Exception

import XML.DOM
import XML.XPath


queryE : String
      -> Document ty
      -> {auto prf : CanQuery ty}
      -> Eff (Either XPathError (List XMLNode)) xs
queryE qstr doc = pure $ query qstr doc


-- --------------------------------------------------------------------- [ EOF ]
