-- ---------------------------------------------------------------- [ Show.idr ]
-- Module      :Show.idr
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
--
-- --------------------------------------------------------------------- [ EOH ]
module XML.DOM.ShowXML

import XML.DOM.Model

-- ------------------------------------------------------------------- [ QName ]
instance [xmlQName] Show QName where
  show (MkQName n ns pre) = n

instance [xmlKV] Show (QName, String) where
  show (k,v) = unwords [" ", show @{xmlQName} k,"=", show v]

instance [xml] Show (Document x) where
  show (MkDocument info dtype ins doc es) = unwords [show @{xml} es]
  show (Element naam as ns) = unwords [
          "<" ++ show @{xmlQName} naam ++ concatMap (show @{xmlKV}) as ++ ">",
          show @{xml} ns,
          "</"++ show @{xmlQName} naam ++ ">\n"]
  show Nil               = ""
  show (x::xs)           = show @{xml} x ++ show @{xml} xs
  show (Comment str)     = unwords ["<!-- ", show str, "-->\n"]
  show (Text txt)        = txt
  show (CData txt)       = unwords ["<![CData[\n", show txt, "\n]]>\n"]
  show (Instruction t d) = unwords ["<?", t , show d ,"?>\n"]

-- --------------------------------------------------------------------- [ EOF ]
