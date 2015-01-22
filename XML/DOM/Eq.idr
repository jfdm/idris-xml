-- ------------------------------------------------------------------ [ Eq.idr ]
-- Module    : Eq.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module XML.DOM.Eq

import XML.DOM.Model

instance Eq QName where
  (==) (MkQName x xs _) (MkQName y ys _) = x == y && xs == ys


instance Eq XMLInfo where
  (==) (MkXMLInfo a b c) (MkXMLInfo x y z) = a == x && b == y && c == z

instance Eq ExternalID where
  (==) (SystemID x)      (SystemID y)      = x == y
  (==) (PublicID x xloc) (PublicID y yloc) = x == y && xloc == yloc
  (==) _                 _                 = False

instance Eq DocType where
  (==) (MkDocType x xid) (MkDocType y yid) = x == y && xid == yid

instance Eq NodeTy where
  (==) DOCUMENT    DOCUMENT    = True
  (==) ELEMENT     ELEMENT     = True
  (==) TEXT        TEXT        = True
  (==) CDATA       CDATA       = True
  (==) INSTRUCTION INSTRUCTION = True
  (==) COMMENT     COMMENT     = True
  (==) NODE        NODE        = True
  (==) _          _            = False

mutual
  %assert_total
  eqDoc : (Document a) -> (Document b) -> Bool
  eqDoc (MkDocument ai ad ais ac ae) (MkDocument bi bd bis bc be) = ai == bi && ad == ad && ais == bis && ac == bc && ae == be
  eqDoc (Element an aa as)  (Element bn ba bs)  = an == bn && aa == ba && as == bs
  eqDoc (Comment a)         (Comment b)         = a == b
  eqDoc (Text a)            (Text b)            = a == b
  eqDoc (CData a)           (CData b)           = a == b
  eqDoc (Instruction at ad) (Instruction bt bd) = at == bt && ad == bd
  eqDoc (Node a)            (Node b)            = eqDoc a b
  eqDoc _                   _                   = False

  --- Eq on liss of nodes is removed.
  instance Eq (Document x) where
    (==) = eqDoc

-- --------------------------------------------------------------------- [ EOF ]
