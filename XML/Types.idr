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
  show (MkQName n ns pre) = unwords ["[", showPre, n, showNS, "]\n"]
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

-- ------------------------------------------------------------ [ Instructions ]
-- <?target data?> SHould data be kv list?
record Instruction : Type where
  MkInstruction : (target : String) -> (kvpairs : List (String, String)) -> Instruction

instance Show Instruction where
  show (MkInstruction t d) = unwords ["[?", t , show d ,"?]\n"]

instance Eq Instruction where
  (==) (MkInstruction x xs) (MkInstruction y ys) = x == y && xs == ys

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

data NodeTy = DOCUMENT | ELEMENT | TEXT | CDATA | INSTRUCTION | COMMENT

||| We are not going to recognise processing instructions.
data ValidNode : NodeTy -> Type where
  ValidElem  : ValidNode ELEMENT
  ValidCData : ValidNode CDATA
  ValidText  : ValidNode TEXT
  ValidDoc   : ValidNode COMMENT


mutual
  data NodeList : Type where
    Nil : NodeList
    (::) : (n : Document a) -> NodeList -> {auto prf : ValidNode a} -> NodeList


  data Document : NodeTy -> Type where
    MkDocument : XMLInfo
               -> Maybe DocType
               -> List Instruction
               -> Document COMMENT
               -> Document ELEMENT
               -> Document DOCUMENT
    Element : QName
            -> List (QName, String)
            -> NodeList
            -> Document ELEMENT
    Comment : String -> Document COMMENT
    Text : String -> Document TEXT
    CData : String -> Document CDATA

  instance Show NodeList where
    show Nil = "[]"
    show ((::) x xs) = show x ++ show xs

  instance Show (Document x) where
    show (MkDocument info dtype ins doc es) = unwords ["[Document ", show info, show dtype, show ins, show doc, show es,"]\n"]
    show (Element naam as ns) = unwords ["[Element ", show naam, show as, show ns, "]\n"]
    show (Comment str) = unwords ["[Comment ", show str, "]\n"]
    show (Text txt) = unwords ["[Text ", show txt, "]\n"]
    show (CData txt) =  unwords ["[CData ", show txt, "]\n"]

-- --------------------------------------------------------------------- [ EOF ]
