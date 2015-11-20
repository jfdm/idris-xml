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
  show (Element naam as Nil) =
    concat [ "<"
           , show @{xmlQName} naam
           , concatMap (show @{xmlKV}) as
           , "/>"]
  show (Element naam as ns)  =
    concat [ "<"
           , show @{xmlQName} naam
           , " "
           , concatMap (show @{xmlKV}) as
           , ">"
           , unwords $ map (show @{xml}) ns
           , "</"
           , show @{xmlQName} naam
           , ">\n"
           ]
  show (Node n)          = show @{xml} n
  show (Comment str)     = unwords ["<!-- ", show str, "-->\n"]
  show (Text txt)        = txt
  show (CData txt)       = unlines ["<![CDATA[", txt, "]]>"]
  show (Instruction t d) = unwords ["<?", t , show d ,"?>\n"]

-- --------------------------------------------------------------------- [ EOF ]
