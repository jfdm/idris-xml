-- ---------------------------------------------------------------- [ Show.idr ]
-- Module      :Show.idr
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
--
-- --------------------------------------------------------------------- [ EOH ]
module XML.DOM.Show

import XML.DOM.Model

%access public export
-- ------------------------------------------------------------------- [ QName ]
implementation Show QName where
  show (MkQName n ns pre) = "[" ++ showPre ++ n ++ showNS ++ "]"
    where
      showPre = case pre of
        Just p => p ++ ":"
        Nothing => ""
      showNS = case ns of
        Just ns' => case pre of
          Just p => " xmlns:" ++ p ++ "=" ++ ns'
          Nothing => " xmlns=" ++ ns'
        Nothing => ""

implementation Show XMLInfo where
  show (MkXMLInfo v enc std) = unwords ["[XML", show v, show enc, show std, "]\n"]

implementation Show ExternalID where
  show (SystemID loc)       = unwords ["SYSTEM", show loc]
  show (PublicID ident loc) = unwords ["PUBLIC", show ident, show loc]

implementation Show DocType where
  show (MkDocType n ident) = unwords ["[!DOCTYPE", n, show ident, "]\n"]

implementation Show (Document x) where
  show (MkDocument info dtype ins doc es) = unwords ["[Document ",
                   show info, show dtype, show ins, show doc, show es,"]\n"]
  show (Element naam as ns) = unwords ["[Element ", show naam, show as, show ns, "]\n"]
  show (Node n)             = show n
  show (Comment str)        = unwords ["[Comment ", show str, "]\n"]
  show (Text txt)           = unwords ["[Text ", show txt, "]\n"]
  show (CData txt)          = unwords ["[CData ", show txt, "]\n"]
  show (Instruction t d)    = unwords ["[?", t , show d ,"?]\n"]
