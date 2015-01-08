-- --------------------------------------------------------------- [ Types.idr ]
-- Module      : Types
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- Types representing XML documents,
-- --------------------------------------------------------------------- [ EOH ]
module XML.Types

-- ------------------------------------------------------------------ [ QNames ]

record QName : Type where
  MkQName : (name : String)
          -> (nspace : Maybe String)
          -> (nprefix : Maybe String)
          -> QName

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

instance Eq QName where
  (==) (MkQName x xs _) (MkQName y ys _) = x == y && xs == ys

-- ---------------------------------------------------------------- [ XML Info ]
data XMLInfo : Type where
  MkXMLInfo : (version : String) -> (encoding : String) -> (standalone : Bool) -> XMLInfo

instance Show XMLInfo where
  show (MkXMLInfo v enc std) = unwords ["[XML", show v, show enc, show std, "]\n"]

instance Eq XMLInfo where
  (==) (MkXMLInfo a b c) (MkXMLInfo x y z) = a == x && b == y && c == z

-- --------------------------------------------------------------- [ Doc Types ]

data ExternalID : Type where
  SystemID : (ident : String) -> ExternalID
  PublicID : (ident : String) -> (ident_sys : String) -> ExternalID

instance Show ExternalID where
  show (SystemID loc)       = unwords ["SYSTEM", show loc]
  show (PublicID ident loc) = unwords ["PUBLIC", show ident, show loc]

instance Eq ExternalID where
  (==) (SystemID x)      (SystemID y)      = x == y
  (==) (PublicID x xloc) (PublicID y yloc) = x == y && xloc == yloc
  (==) _                 _                 = False

data DocType : Type where
  MkDocType : (name : String)
          -> (ident : Maybe ExternalID)
          -> DocType

instance Show DocType where
  show (MkDocType n ident) = unwords ["[!DOCTYPE", n, show ident, "]\n"]

instance Eq DocType where
  (==) (MkDocType x xid) (MkDocType y yid) = x == y && xid == yid

-- ------------------------------------------------------------------- [ Nodes ]

data NodeTy = DOCUMENT | ELEMENT | TEXT | CDATA | INSTRUCTION | COMMENT | NODES

instance Eq NodeTy where
  (==) DOCUMENT    DOCUMENT    = True
  (==) ELEMENT     ELEMENT     = True
  (==) TEXT        TEXT        = True
  (==) CDATA       CDATA       = True
  (==) INSTRUCTION INSTRUCTION = True
  (==) COMMENT     COMMENT     = True
  (==) NODES       NODES       = True
  (==) _          _            = False

||| We are not going to recognise processing instructions.
data ValidNode : NodeTy -> Type where
  ValidElem  : ValidNode ELEMENT
  ValidCData : ValidNode CDATA
  ValidText  : ValidNode TEXT
  ValidDoc   : ValidNode COMMENT
  ValidInstr : ValidNode INSTRUCTION

data Document : NodeTy -> Type where
  MkDocument : XMLInfo
             -> Maybe DocType
             -> List (Document INSTRUCTION)
             -> Maybe (Document COMMENT)
             -> Document ELEMENT
             -> Document DOCUMENT
  Element : QName
          -> List (QName, String)
          -> Document NODES
          -> Document ELEMENT
  Comment : String -> Document COMMENT
  Text : String -> Document TEXT
  CData : String -> Document CDATA
  Instruction : String -> List (String, String) -> Document INSTRUCTION
  Nil : Document NODES
  (::) : Document a -> {auto prf : ValidNode a} -> Document NODES -> Document NODES


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

mutual
  %assert_total
  eqDoc : (Document a) -> (Document b) -> Bool
  eqDoc (MkDocument ai ad ais ac ae) (MkDocument bi bd bis bc be) = ai == bi && ad == ad && ais == bis && ac == bc && ae == be
  eqDoc (Element an aa as)  (Element bn ba bs)  = an == bn && aa == ba && as == bs
  eqDoc (Comment a)         (Comment b)         = a == b
  eqDoc (Text a)            (Text b)            = a == b
  eqDoc (CData a)           (CData b)           = a == b
  eqDoc (Instruction at ad) (Instruction bt bd) = at == bt && ad == bd
  eqDoc Nil                 Nil                 = True
  eqDoc (x::xs)             (y::ys)             = eqDoc x y && eqDoc xs ys
  eqDoc _                   _                   = False

  --- Eq on liss of nodes is removed.
  instance Eq (Document x) where
    (==) = eqDoc

-- ------------------------------------------------------------ [ Node Aliases ]
setRoot : Document ELEMENT -> Document a -> Document DOCUMENT
setRoot newe (MkDocument info dtype ins doc e) = MkDocument info dtype ins doc newe

-- --------------------------------------------------------------------- [ EOF ]
