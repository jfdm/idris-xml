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

createXMLNodeDefault : XMLNode
createXMLNodeDefault = MkXMLNode "1.2" "UTF-8" True

createXMLNode : String -> String -> Bool -> XMLNode
createXMLNode v e a = MkXMLNode v e a

-- --------------------------------------------------------------------- [ EOF ]
