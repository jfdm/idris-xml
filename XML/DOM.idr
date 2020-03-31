-- ----------------------------------------------------------------- [ DOM.idr ]
-- Module      : XML.DOM
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
--
-- --------------------------------------------------------------------- [ EOH ]
module XML.DOM

import public Data.DList
import public Data.PList
import public Commons.Text.Display

%access export
%default total

public export
data NodeTy = DOCUMENT | ELEMENT | TEXT | CDATA | INSTRUCTION | COMMENT | QNAME | INFO | IDENT | DOCTYPE

Eq NodeTy where
  (==) DOCUMENT    DOCUMENT    = True
  (==) ELEMENT     ELEMENT     = True
  (==) TEXT        TEXT        = True
  (==) CDATA       CDATA       = True
  (==) INSTRUCTION INSTRUCTION = True
  (==) COMMENT     COMMENT     = True
  (==) QNAME       QNAME       = True
  (==) INFO        INFO        = True
  (==) IDENT       IDENT       = True
  (==) DOCTYPE     DOCTYPE     = True
  (==) _ _ = False

public export
data ValidNode : NodeTy -> Type where
  ValidElem  : ValidNode ELEMENT
  ValidCData : ValidNode CDATA
  ValidText  : ValidNode TEXT
  ValidDoc   : ValidNode COMMENT
  ValidInstr : ValidNode INSTRUCTION

data Document : NodeTy -> Type where
  MkDocument : (info         : Document INFO)
            -> (doctype      : Maybe $ Document DOCTYPE)
            -> (instructions : List (Document INSTRUCTION))
            -> (comment      : Maybe (Document COMMENT))
            -> (root         : Document ELEMENT)
            -> Document DOCUMENT

  XMLInfo : (version : String)
         -> (encoding : String)
         -> (standalone : Bool)
         -> Document INFO

  DocType : (name : String)
         -> (ident : Maybe $ Document IDENT)
         -> Document DOCTYPE

  SystemID : (ident : String) -> Document IDENT
  PublicID : (ident : String) -> (ident_sys : String) -> Document IDENT

  QName : (name : String)
       -> (nspace : Maybe String)
       -> (nprefix : Maybe String)
       -> Document QNAME


  Element : (qName      : Document QNAME)
         -> (attributes : List (Document QNAME, String))
         -> (children   : PList NodeTy Document ValidNode ts prfs)
         -> Document ELEMENT

  Comment : (comment : String) -> Document COMMENT

  Text : (text : String) -> Document TEXT

  CData : (cdata : String) -> Document CDATA

  Instruction : (name : String)
             -> (attributes : List (String, String))
             -> Document INSTRUCTION

-- ---------------------------------------------------------------------- [ Eq ]

private
maybeEq : (Document a -> Document b -> Bool)
       -> Maybe (Document a)
       -> Maybe (Document b)
       -> Bool
maybeEq _ Nothing  Nothing  = True
maybeEq f (Just x) (Just y) = f x y
maybeEq _ _        _        = False

partial
eqDoc : Document a -> Document b -> Bool
eqDoc (MkDocument iA dA inA cA rA) (MkDocument iB dB inB cB rB) =
         eqDoc iA iB
      && maybeEq eqDoc dA dB
      && (and (zipWith (\x, y => eqDoc x y) inA inB))
      && maybeEq eqDoc cA cB
      && eqDoc rA rB

eqDoc (XMLInfo vA eA sA) (XMLInfo vB eB sB) = vA == vB && eA == eB && sA == sB

eqDoc (DocType nA iA) (DocType nB iB) = nA == nB && maybeEq eqDoc iA iB

eqDoc (SystemID x) (SystemID y) = x == y

eqDoc (PublicID x xloc) (PublicID y yloc) = x == y && xloc == yloc

eqDoc (QName x xs _) (QName y ys _) = x == y && xs == ys

eqDoc (Element nA aA cA) (Element nB aB cB) =
        eqDoc nA nB
     && (and (zipWith (\(a,b), (x,y) => eqDoc a x && b == y) aA aB))
     && eqPList eqDoc cA cB

eqDoc (Comment a) (Comment b) = a == b

eqDoc (Text a) (Text b) = a == b
eqDoc (CData a) (CData b) = a == b
eqDoc (Instruction at ad) (Instruction bt bd) = at == bt && ad == ad
eqDoc _ _ = False



-- -------------------------------------------------------------------- [ Show ]

partial
Show (Document a) where
  show (MkDocument info doctype instructions comment root) =
      unwords ["[MkDocument"
              , show info
              , show doctype
              , show instructions
              , show comment
              , show root
              ,"]"
              ]
  show (XMLInfo version encoding standalone) =
      unwords ["[XMLInfo"
              , show version
              , show encoding
              , show standalone
              , "]"
              ]
  show (DocType name ident) =
      unwords ["[DocType"
              , show name
              , show ident
              , "]"
              ]
  show (SystemID ident) = unwords ["SystemID", show ident]

  show (PublicID ident loc) = unwords ["PublicID", show ident, show loc]

  show (QName name nspace nprefix) =
      unwords ["[QName"
              , show name
              , show nspace
              , show nprefix
              , "]"
              ]

  show (Element qName attributes children) =
       unwords ["[Element "
               , show qName
               , show attributes
               , showPList show children
               , "]"
               ]

  show (Comment comment) = unwords ["[Comment ", show comment, "]"]
  show (Text text) = unwords ["[Text", show text, "]"]
  show (CData cdata) = unwords ["[CData", show cdata, "]"]

  show (Instruction name attributes) =
       unwords ["[Instruction"
               , show name
               , show attributes
               ,"]"
               ]

partial
displayDoc : Document a -> String
displayDoc (MkDocument info doctype instructions comment root) =
    unwords ["[MkDocument"
            , displayDoc info
            , maybe "" displayDoc doctype
            , unwords $ Functor.map displayDoc instructions
            , maybe "" displayDoc comment
            , displayDoc root
            ,"]"
            ]
displayDoc (XMLInfo version encoding standalone) =
    unwords ["<?xml"
            , concat ["version=", show version]
            , concat ["encoding=", show encoding]
--            , concat ["standalone=", show standalone
            , "?>"
            ]
displayDoc (DocType name ident) =
    unwords ["<!DOCTYPE"
            , name
            , maybe "" displayDoc ident
            , ">"
            ]
displayDoc (SystemID ident) = ident

displayDoc (PublicID ident loc) = unwords [display ident, display loc]

displayDoc (QName name nspace nprefix) =
    concat [maybe "" display nspace
           , if isJust nspace then ":" else ""
           , display name
           ]

displayDoc (Element qName attributes Nil) =
         concat [ "<"
                , displayDoc qName
                , unwords $ Functor.map (\(k,v) => concat [displayDoc k, "=", show v]) attributes
                , "/>"
                ]

displayDoc (Element qName attributes children) =
         concat ["[Element "
                , "<", displayDoc qName, ">"
                , unwords $ Functor.map (\(k,v) => concat [displayDoc k, "=", show v]) attributes
                , concat $ map displayDoc children
                ,"</", displayDoc qName, ">"
                ]

displayDoc (Comment comment) = unwords ["<!--", comment, "-->"]
displayDoc (Text text) = text
displayDoc (CData cdata) = unwords ["<![CData[", cdata, "]]>"]

displayDoc (Instruction name attributes) =
     unwords ["<?"
             , name
             , show attributes
             ,"?>"
             ]

Display (Document a) where
  display d = assert_total $ displayDoc d -- nasty hack
-- -------------------------------------------------------------------- [ Misc ]

getDocElemTy : {a : NodeTy} -> Document a -> NodeTy
getDocElemTy {a} _ = a


public export
NodeList : (types : List NodeTy)
        -> (prfs  : DList NodeTy ValidNode types)
        -> Type
NodeList = PList NodeTy Document ValidNode
-- --------------------------------------------------------------------- [ API ]

setRoot : Document ELEMENT -> Document DOCUMENT -> Document DOCUMENT
setRoot newe (MkDocument info dtype ins doc e) = MkDocument info dtype ins doc newe

getRoot : Document DOCUMENT -> Document ELEMENT
getRoot (MkDocument info doctype instructions comment root) = root

mkXMLInfo : String -> String -> Bool -> Document INFO
mkXMLInfo = XMLInfo

defaultXMLInfo : Document INFO
defaultXMLInfo = mkXMLInfo "1.2" "UTF-8" True

emptyNodeList : PList NodeTy Document ValidNode Nil Nil
emptyNodeList = Nil

mkSystemID : String -> Document IDENT
mkSystemID = SystemID

mkPublicID : String -> String -> Document IDENT
mkPublicID = PublicID

-- [ DocTypes ]

mkDocType : String -> Maybe (Document IDENT) -> Document DOCTYPE
mkDocType = DocType

-- ------------------------------------------------------------- [ DOM Objects ]

-- ------------------------------------------------------------------ [ QNames ]

mkQName : String -> Maybe String -> Maybe String -> Document QNAME
mkQName = QName

namespace Simple

    ||| Create a local name
    mkQName : String -> Document QNAME
    mkQName n = QName n Nothing Nothing

||| Create a qualified name with a name space
|||
||| @n The name
||| @ns   The name space.
mkQNameNS : (n : String) -> (ns : String) -> Document QNAME
mkQNameNS n ns = QName n (Just ns) Nothing

||| Create a tag with a name and a prefix
|||
||| @n The name
||| @pre The prefix
mkQNamePrefix : (n : String) -> (pre : String) -> Document QNAME
mkQNamePrefix n pre = QName n Nothing (Just pre)

||| Create a tag with namespace and prefix.
|||
||| @n The name
||| @pre The prefix.
||| @ns The namespace
mkQNameNSPrefix : (n : String) -> (pre : String) -> (ns : String) -> Document QNAME
mkQNameNSPrefix n pre ns = QName n (Just ns) (Just pre)

||| Create a prefixed qualified name, intended for use with
||| attributes.
mkAttrNamePrefix : String -> String -> Document QNAME
mkAttrNamePrefix n pre = QName n Nothing (Just pre)

setNameSpace : Maybe String -> Document QNAME -> Document QNAME
setNameSpace s (QName n _ pre) = QName n s pre

-- ---------------------------------------------------------------- [ Elements ]

||| Create a element with a local qualified name.
mkSimpleElement : String -> Document ELEMENT
mkSimpleElement name = Element tag Nil Nil
  where
   tag : Document QNAME
   tag = mkQName name

||| Create a element with a Namespace.
|||
||| @n The name
||| @ns The namespace
mkElementNS : (n : String) -> (ns : String) -> Document ELEMENT
mkElementNS n ns = Element tag Nil Nil
  where
    tag = mkQNameNS n ns

||| Create an element with a prefix
|||
||| @n The name
||| @pre The prefix.
mkElementPrefix : (n : String) -> (pre : String) -> Document ELEMENT
mkElementPrefix n p = Element (mkQNamePrefix n p) Nil Nil

||| Create an element with a namespace and a prefix
|||
||| @n The name
||| @pre The prefix.
||| @ns The namespace
mkElementNSPrefix : (n : String)
                 -> (pre : String)
                 -> (ns : String)
                 -> Document ELEMENT
mkElementNSPrefix n pre ns = Element (mkQNameNSPrefix n pre ns) Nil Nil

||| Create a key value pair
mkAttribute : String -> String -> (Document QNAME, String)
mkAttribute k v = (mkQName k, v)

mkAttributePrefix : String -> String -> String -> (Document QNAME, String)
mkAttributePrefix k p v = (mkQNamePrefix k p, v)

||| Creates a Document with an empty root node
mkEmptyDocument : Document QNAME -> Maybe (Document DOCTYPE) -> Document DOCUMENT
mkEmptyDocument n dtd =
  MkDocument defaultXMLInfo
             dtd
             Nil
             Nothing
             (Element n Nil emptyNodeList)

mkSimpleDocument : Document ELEMENT -> Document DOCUMENT
mkSimpleDocument root = MkDocument (defaultXMLInfo) Nothing Nil Nothing root

mkDocument : (info         : Document INFO)
          -> (doctype      : Maybe $ Document DOCTYPE)
          -> (instructions : List (Document INSTRUCTION))
          -> (comment      : Maybe (Document COMMENT))
          -> (root         : Document ELEMENT)
          -> Document DOCUMENT
mkDocument = MkDocument

-- ----------------------------------------------------------- [ Node Creation ]

||| Create an XML Comment
mkComment : String -> Document COMMENT
mkComment = Comment

mkCData : String -> Document CDATA
mkCData = CData

mkText : String -> Document TEXT
mkText = Text

mkInstruction : String -> List (String, String) -> Document INSTRUCTION
mkInstruction = Instruction

mkEmptyElement : Document QNAME -> List (Document QNAME, String) -> Document ELEMENT
mkEmptyElement n as = Element n as Nil

mkElement : Document QNAME -> List (Document QNAME, String) -> NodeList ts prfs -> Document ELEMENT
mkElement = Element

-- -------------------------------------------------------------- [ Attributes ]

getAttrName : (Document QNAME, String) -> String
getAttrName (QName n ns prefix', v) = n

getAttrPrefix : (Document QNAME, String) -> Maybe String
getAttrPrefix (QName n ns prefix', v) = prefix'

getAttrValue : (Document QNAME, String) -> String
getAttrValue (k,v) = v

-- --------------------------------------------------------------- [ Accessors ]

||| Get the attributes of the node
getAttributes : (node : Document a )
             -> {auto prf : ValidNode a}
             -> List (Document QNAME, String)
getAttributes (Element x xs y) {prf=ValidElem} = xs
getAttributes node {prf} = Nil

||| Does node have attributes
hasAttributes : Document a
             -> {auto prf : ValidNode a}
             -> Bool
hasAttributes n {prf} = isCons (getAttributes n {prf=prf})

||| Get the children
getNodes : Document a
        -> {auto prf : ValidNode a}
        -> (ts ** prfs ** NodeList ts prfs)
getNodes (Element x xs ys) {prf=ValidElem} = (_ ** _ ** ys)
getNodes x {prf}                           = ([] ** [] ** [])

||| Does element have child nodes
hasNodes : Document a
        -> {auto prf : ValidNode a}
        -> Bool
hasNodes n = let (ts ** prfs ** nodes) = getNodes n
              in isCons nodes

||| Get node name
||| http://docs.oracle.com/javase/7/docs/api/org/w3c/dom/Node.html
getNodeName : Document a
           -> {auto prf : ValidNode a}
           -> String
getNodeName (Element (QName name nspace nprefix) xs y)   {prf = ValidElem}  = name
getNodeName (CData x)          {prf = ValidCData} = "#cdata-section"
getNodeName (Text x)           {prf = ValidText}  = "#text"
getNodeName (Comment x)        {prf = ValidDoc}   = "#comment"
getNodeName (Instruction x xs) {prf = ValidInstr} = x

||| Return the element's value
getNodeValue : Document a
            -> {auto prf : ValidNode a}
            -> Maybe String
getNodeValue x {prf = ValidElem} = Nothing
getNodeValue (CData x) {prf = ValidCData} = Just x
getNodeValue (Text x) {prf = ValidText} = Just x
getNodeValue (Comment x) {prf = ValidDoc} = Just x
getNodeValue (Instruction x xs) {prf = ValidInstr} =
  Just $ unwords $ Functor.map show xs

getTag : Document ELEMENT -> Document QNAME
getTag (Element n _ _) = n

||| Get tag name
getTagName : Document ELEMENT -> String
getTagName (Element (QName name nspace nprefix) _ _) = name

||| Return an element's prefix
getTagPrefix : Document ELEMENT -> Maybe String
getTagPrefix (Element (QName name nspace nprefix) _ _) = nprefix

||| Return an element's namespace
getTagNS : Document ELEMENT -> Maybe String
getTagNS (Element (QName name nspace nprefix) _ _ ) = nspace


||| Get value for a given attribute
getAttribute : String -> Document ELEMENT -> Maybe String
getAttribute key (Element x xs y) =
    lookupBy cmpQName (mkQName key) xs
  where
    cmpQName (QName a _ _) (QName b _ _) = a == b

||| Remove first occurance of attribute.
removeAttribute : String -> Document ELEMENT -> Document ELEMENT
removeAttribute key (Element n as ns) = Element n attrs' ns
  where
    cmpQNameKVPair : (Document QNAME, String)
                  -> (Document QNAME, String)
                  -> Bool
    cmpQNameKVPair (QName a _ _, b) (QName x _ _, y) = a == x

    attrs' : List (Document QNAME, String)
    attrs' = deleteBy cmpQNameKVPair
                      (mkQName key, "")
                      (as)

||| Set first occurance of atttribute to new value.
setAttribute : (key : String)
            -> (value : String)
            -> Document ELEMENT
            -> Document ELEMENT
setAttribute k v e@(Element n as ns) = Element n (newAS e) ns
  where
    newAS : Document ELEMENT -> List (Document QNAME, String)
    newAS e = mkAttribute k v :: getAttributes (removeAttribute k e)


-- ------------------------------------------------------------- [ Element Ops ]

ContainsChild : (child : Document a)
             -> (node  : NodeList ts prfs)
             -> (prfI : Elem a ts)
             -> (prfP : DElem NodeTy ValidNode prf prfs prfI)
             -> Type
ContainsChild c n prfI prfP {a} {prf} =
  Elem NodeTy Document ValidNode a c prf n prfI prfP

data HasElem : (elem : Document a)
            -> (prf : ValidNode a)
            -> (node : Document ELEMENT)
            -> Type
  where
    HasElemProof : (prf : ContainsChild elem children prfI prfP)
                -> HasElem elem vnode (Element n as children)

infixl 2 <++> -- Append Child
infixl 2 <--> -- Remove Child
infixl 2 <=>  -- Add Text Node
infixl 2 <+=> -- Create and add text node

private
appendToNode : Document a
            -> Document ELEMENT
            -> (prf : ValidNode a)
            -> Document ELEMENT
appendToNode c (Element n as ns) prf = Element n as (add c ns)

private
removeFromNodeList : (elem : Document a)
                  -> (nodes : NodeList ts prfs)
                  -> {auto idx  : Elem a ts}
                  -> {auto vprf : DElem NodeTy ValidNode prf prfs idx}
                  -> NodeList (dropElem ts idx) (dropElem prfs vprf)
removeFromNodeList elem nodes {idx} {vprf} = delete' elem nodes idx vprf

private
removeFromNode : (elem : Document a)
              -> (node : Document ELEMENT)
              -> (vnode : ValidNode a)
              -> (prf : HasElem elem vnode node)
              -> Document ELEMENT
removeFromNode c (Element n as ns {ts = ts}) vnode (HasElemProof prf) =
   Element n as (dropElem ns prf)

||| Set Value
addText : String -> Document ELEMENT -> Document ELEMENT
addText s e = appendToNode (Text s) e ValidText

--  -------------------------------------------------------------------- [ Ops ]

||| Append
(<++>) : Document ELEMENT
       -> Document a
       -> {auto prf : ValidNode a}
       -> Document ELEMENT
(<++>) p c {prf} = appendToNode c p prf

||| Remove
(<-->) : (node : Document ELEMENT)
      -> (elem : Document a)
      -> {auto prfV : ValidNode a}
      -> {auto prfN : HasElem elem prfV node}
      -> Document ELEMENT
(<-->) p c {prfV} {prfN} = removeFromNode c p prfV prfN

||| Add text value
(<=>) : Document ELEMENT -> String -> Document ELEMENT
(<=>) e s = e <++> (Text s)

||| Create and add text value
(<+=>) : String -> String -> Document ELEMENT
(<+=>) n v = (mkSimpleElement n) <=> v

-- ------------------------------------------------------------ [ Node Queries ]

namespace NodeList
  ||| getElements
  getElements : NodeList ts prfs
             -> List $ Document ELEMENT
  getElements [] = []
  getElements ((::) elem {prf = ValidElem} rest) = elem :: getElements rest
  getElements ((::) elem {prf} rest) = getElements rest

  getText : NodeList ts prfs
         -> List $ Document TEXT
  getText [] = []
  getText ((::) elem {prf = ValidText} rest) = elem :: getText rest
  getText ((::) elem {prf} rest) = getText rest

  getComments : NodeList ts prfs
             -> List $ Document COMMENT
  getComments Nil = Nil
  getComments ((::) elem {prf = ValidDoc} rest) = elem :: getComments rest
  getComments ((::) elem {prf} rest)            = getComments rest

  getCData : NodeList ts prfs -> List $ Document CDATA
  getCData Nil = Nil
  getCData ((::) elem {prf = ValidCData} rest) = elem :: getCData rest
  getCData ((::) elem {prf} rest)              = getCData rest

getElements : Document ELEMENT -> List $ Document ELEMENT
getElements (Element _ _ ns) = getElements ns

getText : Document ELEMENT -> List $ Document TEXT
getText (Element _ _ ns) = getText ns

getComments : Document ELEMENT -> List $ Document COMMENT
getComments (Element _ _ ns) = getComments ns

getCData : Document ELEMENT -> List $ Document CDATA
getCData (Element _ _ ns) = getCData ns

-- --------------------------------------------------------- [ Element Queries ]

public export
data CanQuery : NodeTy -> Type where
  QueryDoc  : CanQuery DOCUMENT
  QueryElem : CanQuery ELEMENT

||| Get the immediate child elements
getChildElements : Document a -> {auto prf : CanQuery a} -> List $ Document ELEMENT
getChildElements (Element _ _ ns) {prf=QueryElem} = getElements ns
getChildElements (MkDocument x y xs z (Element _ _ ns)) {prf=QueryDoc}  = getElements ns

private
doTest : (eqFunc : Document QNAME -> Document QNAME -> Bool)
      -> (name   : Document QNAME)
      -> (node   : Document ELEMENT)
      -> Maybe (Document ELEMENT)
doTest eqFunc name node@(Element n attrs children) =
  if eqFunc name n
    then Just node
    else Nothing

private partial
getAllElements : (node : Document ELEMENT)
              -> List $ Document ELEMENT
getAllElements node@(Element x as Nil) = node :: Nil
getAllElements node@(Element x as xs)  = node :: concatMap getAllElements (getElements xs)


private
getElementsBy' : (func : Document ELEMENT -> Bool)
              -> (node : Document a)
              -> (prf  : CanQuery a)
              -> List (Document ELEMENT)
getElementsBy' func (MkDocument x y xs z w) QueryDoc = filter func (assert_total $ getAllElements w)
getElementsBy' func node QueryElem = filter func (assert_total $ getAllElements node)


getElementsBy : (func : Document ELEMENT -> Bool)
             -> (node : Document a)
             -> {auto prf : CanQuery a}
             -> List (Document ELEMENT)
getElementsBy func node {prf} = getElementsBy' func node prf

||| Get all Elements with a given QName
getElementsByQName : Document QNAME
                  -> Document a
                  -> {auto prf : CanQuery a}
                  -> List $ Document ELEMENT
getElementsByQName (QName x ns p) node = getElementsBy (\(Element (QName y _ _) as xs) => x == y) node
-- change tp ==

||| Get all Elements with a given name. This ignores prefixes and namespaces.
getElementsByName : String
                 -> Document a
                 -> {auto prf : CanQuery a}
                 -> List $ Document ELEMENT
getElementsByName naam e = getElementsBy (\(Element (QName x _ _) _ _) => naam == x) e

||| Get All Child Elements with a given QName.
getChildElementsByQName : Document QNAME
                       -> Document a
                       -> {auto prf : CanQuery a}
                       -> List $ Document ELEMENT
getChildElementsByQName (QName y _ _) node {prf} =
  filter (\(Element (QName x _ _) _ _) => x == y) (getChildElements node) -- change to ==

||| Get All Child Elements with a local name
getChildElementsByName : String
                      -> Document a
                      -> {auto prf : CanQuery a}
                      -> List $ Document ELEMENT
getChildElementsByName name node {prf} =
  filter (\(Element (QName x _ _) _ _) => x == name) (getChildElements node)

||| Get All child elements regardless of name.
getAllChildren : Document a
              -> {auto prf : CanQuery a}
              -> List $ Document ELEMENT
getAllChildren (MkDocument x y xs z w) {prf = QueryDoc} = assert_total $ getAllElements w
getAllChildren node {prf = QueryElem} = assert_total $ getAllElements node

-- ----------------------------------------------------------------- [ Aliases ]

public export
XMLDoc : Type
XMLDoc = Document DOCUMENT

public export
XMLElem : Type
XMLElem = Document ELEMENT

-- --------------------------------------------------------------------- [ EOF ]
