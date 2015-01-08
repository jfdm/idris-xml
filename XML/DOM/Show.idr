-- ---------------------------------------------------------------- [ Show.idr ]
-- Module      :Show.idr
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
--
-- --------------------------------------------------------------------- [ EOH ]
module XML.DOM.Show

import XML.DOM.Model

-- ------------------------------------------------------------------- [ QName ]
instance Show QName where
  show (MkQName n ns pre) = unwords ["[", showPre, n, showNS, "]"]
    where
      showPre = case pre of
        Just p => p ++ ":"
        Nothing => ""
      showNS = case ns of
        Just n => case pre of
          Just p => unwords ["xmlns:",p,"=",n]
          Nothing => unwords ["xmlns=",n]
        Nothing => ""

instance Show XMLInfo where
  show (MkXMLInfo v enc std) = unwords ["[XML", show v, show enc, show std, "]\n"]

instance Show ExternalID where
  show (SystemID loc)       = unwords ["SYSTEM", show loc]
  show (PublicID ident loc) = unwords ["PUBLIC", show ident, show loc]

instance Show DocType where
  show (MkDocType n ident) = unwords ["[!DOCTYPE", n, show ident, "]\n"]

instance Show (Document x) where
  show (MkDocument info dtype ins doc es) = unwords ["[Document ",
                   show info, show dtype, show ins, show doc, show es,"]\n"]
  show (Element naam as ns) = unwords ["[Element ", show naam, show as, showNS ns, "]\n"]
    where
      showNS : Document NODES -> String
      showNS (x::xs) = show x ++ "," ++ showNS xs
  show (Comment str) = unwords ["[Comment ", show str, "]\n"]
  show (Text txt) = unwords ["[Text ", show txt, "]\n"]
  show (CData txt) =  unwords ["[CData ", show txt, "]\n"]
  show (Instruction t d) = unwords ["[?", t , show d ,"?]\n"]
