-- ----------------------------------------------------------------- [ DOM.idr ]
-- Module      : XML.DOM
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
--
-- --------------------------------------------------------------------- [ EOH ]
module XML.DOM

import public XML.DOM.Model
import public XML.DOM.Show
import public XML.DOM.ShowXML
import public XML.DOM.Eq
import public XML.DOM.Utils
import public XML.DOM.API

%access public export

XMLDoc : Type
XMLDoc = Document DOCUMENT

XMLElem : Type
XMLElem = Document ELEMENT

XMLNode : Type
XMLNode = Document NODE

-- --------------------------------------------------------------------- [ EOF ]
