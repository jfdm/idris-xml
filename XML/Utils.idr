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

-- --------------------------------------------------------------------- [ EOF ]
