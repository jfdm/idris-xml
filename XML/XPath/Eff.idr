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

export
queryE : String
      -> Document ty
      -> {auto prf : CanQuery ty}
      -> Eff (Either XPathError (ty' ** XPathResult ty')) xs
queryE qstr doc {ty=ELEMENT} = pure (queryElem qstr doc)
queryE qstr doc {ty=DOCUMENT} = pure (queryDoc qstr doc)


-- --------------------------------------------------------------------- [ EOF ]
