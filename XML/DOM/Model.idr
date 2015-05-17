-- --------------------------------------------------------------- [ Model.idr ]
-- Module      : XML.DOM.Model
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- Types representing XML documents,
-- --------------------------------------------------------------------- [ EOH ]
module XML.DOM.Model

-- ------------------------------------------------------------------ [ QNames ]

record QName where
  constructor MkQName
  name : String
  nspace : Maybe String
  nprefix : Maybe String

-- ---------------------------------------------------------------- [ XML Info ]
record XMLInfo where
  constructor MkXMLInfo
  version : String
  encoding : String
  standalone : Bool

-- --------------------------------------------------------------- [ Doc Types ]

data ExternalID : Type where
  SystemID : (ident : String) -> ExternalID
  PublicID : (ident : String) -> (ident_sys : String) -> ExternalID

data DocType : Type where
  MkDocType : (name : String)
          -> (ident : Maybe ExternalID)
          -> DocType

-- ------------------------------------------------------------------- [ Nodes ]

data NodeTy = DOCUMENT | ELEMENT | TEXT | CDATA | INSTRUCTION | COMMENT | NODE

data ValidNode : NodeTy -> Type where
  ValidElem  : ValidNode ELEMENT
  ValidCData : ValidNode CDATA
  ValidText  : ValidNode TEXT
  ValidDoc   : ValidNode COMMENT
  ValidInstr : ValidNode INSTRUCTION

syntax IsOK = tactics { search 100; }

data Document : NodeTy -> Type where
  MkDocument : XMLInfo
             -> Maybe DocType
             -> List (Document INSTRUCTION)
             -> Maybe (Document COMMENT)
             -> Document ELEMENT
             -> Document DOCUMENT
  Element : QName
          -> List (QName, String)
          -> List (Document NODE)
          -> Document ELEMENT
  Comment : String -> Document COMMENT
  Text : String -> Document TEXT
  CData : String -> Document CDATA
  Instruction : String -> List (String, String) -> Document INSTRUCTION
  Node : Document ty -> {auto prf : ValidNode ty} -> Document NODE

getDocElemTy : {a : NodeTy} -> Document a -> NodeTy
getDocElemTy {a} _ = a

-- --------------------------------------------------------------------- [ EOF ]
