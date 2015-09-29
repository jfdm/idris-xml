-- ----------------------------------------------------------------- [ DOM.idr ]
-- Module      :
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- A DOM Style API to XML data inspired by
-- `https://docs.python.org/2/library/xml.dom.html`
-- --------------------------------------------------------------------- [ EOH ]
module XML.DOM.API

import XML.DOM.Model
import XML.DOM.Show
import XML.DOM.Eq
import XML.DOM.Utils

import Debug.Trace

%access public

-- ---------------------------------------------------------------- [ DocTypes ]

mkDocType : String -> Maybe ExternalID -> DocType
mkDocType name id = MkDocType name id

-- ------------------------------------------------------------- [ DOM Objects ]

||| Creates a Document with an empty root node
mkEmptyDocument : QName -> Maybe DocType -> Document DOCUMENT
mkEmptyDocument n dtd = MkDocument (createXMLInfoDefault) dtd Nil Nothing
                              (Element n Nil Nil)

mkDocument : Document ELEMENT -> Document DOCUMENT
mkDocument root = MkDocument (createXMLInfoDefault) Nothing Nil Nothing root

-- ------------------------------------------------------------------ [ QNames ]

||| Create a local name
mkQName : String -> QName
mkQName n = MkQName n Nothing Nothing

||| Create a qualified name with a name space
|||
||| @n The name
||| @ns   The name space.
mkQNameNS : (n : String) -> (ns : String) -> QName
mkQNameNS n ns = MkQName n (Just ns) Nothing

||| Create a tag with a name and a prefix
|||
||| @n The name
||| @pre The prefix
mkQNamePrefix : (n : String) -> (pre : String) -> QName
mkQNamePrefix n pre = MkQName n Nothing (Just pre)

||| Create a tag with namespace and prefix.
|||
||| @n The name
||| @pre The prefix.
||| @ns The namespace
mkQNameNSPrefix : (n : String) -> (pre : String) -> (ns : String) -> QName
mkQNameNSPrefix n pre ns = MkQName n (Just ns) (Just pre)

||| Create a prefixed qualified name, intended for use with
||| attributes.
mkAttrNamePrefix : String -> String -> QName
mkAttrNamePrefix n pre = MkQName n Nothing (Just pre)

-- ---------------------------------------------------------------- [ Elements ]

||| Create a element with a local qualified name.
mkSimpleElement : String -> Document ELEMENT
mkSimpleElement name = Element tag Nil Nil
  where
   tag : QName
   tag = mkQName name

mkNode : String -> Document ELEMENT
mkNode = mkSimpleElement

||| Create a element with a qualified name.
mkElement : QName -> Document ELEMENT
mkElement qname = Element qname Nil Nil

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
mkAttribute : String -> String -> (QName, String)
mkAttribute k v = (mkQName k, v)

mkAttributePrefix : String -> String -> String -> (QName, String)
mkAttributePrefix k p v = (mkQNamePrefix k p, v)

-- ----------------------------------------------------------- [ Node Creation ]

||| Create an XML Comment
mkCommentNode : String -> Document NODE
mkCommentNode txt = Node $ Comment txt

mkCData : String -> Document NODE
mkCData txt = Node $ CData txt

mkTextNode : String -> Document NODE
mkTextNode txt = Node $ Text txt

mkInstructNode : String -> List (String, String) -> Document NODE
mkInstructNode t d = Node $ Instruction t d

mkElementNode : String -> Document NODE
mkElementNode s = Node $ mkSimpleElement s

-- -------------------------------------------------------------- [ Attributes ]

getAttrName : (QName, String) -> String
getAttrName (k,v) = name k

getAttrPrefix : (QName, String) -> Maybe String
getAttrPrefix (k,v) = nprefix k

getAttrValue : (QName, String) -> String
getAttrValue (k,v) = v

-- ------------------------------------------------------------- [ Element Ops ]
infixl 2 <++> -- Append Child
infixl 2 <--> -- Remove Child
infixl 2 <=>  -- Add Text Node
infixl 2 <+=> -- Create and add text node

private
appendToNode : Document a
           -> Document ELEMENT
           -> (prf : ValidNode a)
           -> Document ELEMENT
appendToNode c (Element n as ns) prf = Element n as (Node {prf=prf} c :: ns)

private
removeFromNode : Document a
               -> Document ELEMENT
               -> (prf : ValidNode a)
               -> Document ELEMENT
removeFromNode c (Element n as ns) prf = Element n as (delete (Node {prf=prf} c) ns)

||| Set Value
addText : String -> Document ELEMENT -> Document ELEMENT
addText s e = appendToNode (Text s) e IsOK

--  -------------------------------------------------------------------- [ Ops ]

||| Append
(<++>) : Document ELEMENT
       -> Document a
       -> {default IsOK p : ValidNode a}
       -> Document ELEMENT
(<++>) p c = appendToNode c p IsOK

||| Remove
(<-->) : Document ELEMENT -> Document a
       -> {default IsOK p : ValidNode a}
       -> Document ELEMENT
(<-->) p c = removeFromNode c p IsOK

||| Add text value
(<=>) : Document ELEMENT -> String -> Document ELEMENT
(<=>) e s = e <++> (Text s)

||| Create and add text value
(<+=>) : String -> String -> Document ELEMENT
(<+=>) n v = (mkSimpleElement n) <=> v

-- --------------------------------------------------------------- [ Accessors ]
||| Get the attributes of the node
getAttributes : Document a -> List (QName, String)
getAttributes (Node n)         = getAttributes n
getAttributes (Element _ as _) = as
getAttributes _                = Nil

||| Does node have attributes
hasAttributes : Document a -> Bool
hasAttributes n = isCons (getAttributes n)

||| Get the children
getNodes : Document a -> List $ Document NODE
getNodes (Node n)         = getNodes n
getNodes (Element _ _ ns) = ns
getNodes _                = Nil

||| Does element have child nodes
hasNodes : Document a -> Bool
hasNodes n = isCons $ getNodes n

||| Get node name
||| http://docs.oracle.com/javase/7/docs/api/org/w3c/dom/Node.html
getNodeName : Document a -> Maybe String
getNodeName (Node n)           = getNodeName n
getNodeName (CData _)          = Just $ "#cdata-section"
getNodeName (Comment  _)       = Just $ "#comment"
getNodeName (Instruction i _)  = Just $ i
getNodeName (Text _)           = Just $ "#text"
getNodeName (Element naam _ _) = Just $ name naam
getNodeName _                  = Nothing

||| Return the element's value
getNodeValue : Document a -> Maybe String
getNodeValue (Node n)          = getNodeValue n
getNodeValue (CData d)         = Just d
getNodeValue (Comment c)       = Just c
getNodeValue (Instruction _ d) = Just $ concatMap show d
getNodeValue (Text t)          = Just t
getNodeValue (Element _ _ _)   = Nothing
getNodeValue _                 = Nothing

getTag : Document ELEMENT -> QName
getTag (Element n _ _) = n

||| Get tag name
getTagName : Document ELEMENT -> String
getTagName (Element n _ _) = name n

||| Return an element's prefix
getTagPrefix : Document ELEMENT -> Maybe String
getTagPrefix (Element n _ _) = nprefix n

||| Return an element's namespace
getTagNS : Document ELEMENT -> Maybe String
getTagNS (Element n _ _ ) = nspace n

||| Get value for a given attribute
getAttribute : String -> Document ELEMENT -> Maybe String
getAttribute key e = lookupBy (\x,y => name x == name y)
                              (mkQName key)
                              (getAttributes e)

||| Remove first occurance of attribute.
removeAttribute : String -> Document ELEMENT -> Document ELEMENT
removeAttribute key (Element n as ns) = Element n (attrs') ns
  where
    attrs' : List (QName, String)
    attrs' = deleteBy (\(x,y), (a,b) => name x== name a )
                      (mkQName key, "")
                      (as)

||| Set first occurance of atttribute to new value.
setAttribute : (key : String)
             -> (value : String)
             -> Document ELEMENT
             -> Document ELEMENT
setAttribute k v e@(Element n as ns) = Element n (newAS e) ns
  where
    newAS : Document ELEMENT -> List (QName, String)
    newAS e = mkAttribute k v :: getAttributes (removeAttribute k e)

-- ------------------------------------------------------------ [ Node Queries ]

||| getElements
getElements : List $ Document NODE -> List $ Document ELEMENT
getElements Nil     = Nil
getElements (Node x::xs) with (x)
    | (Element _ _ _) = x :: getElements xs
    | otherwise       = getElements xs

getText : List $ Document NODE -> List $ Document TEXT
getText Nil = Nil
getText (Node x::xs) with (x)
    | (Text t)  = x :: getText xs
    | otherwise = getText xs

getComments : List $ Document NODE -> List $ Document COMMENT
getComments Nil = Nil
getComments (Node x::xs) with (x)
    | (Comment t)  = x :: getComments xs
    | otherwise = getComments xs

getCData : List $ Document NODE -> List $ Document CDATA
getCData Nil = Nil
getCData (Node x::xs) with (x)
    | (CData t)  = x :: getCData xs
    | otherwise = getCData xs


||| Get the immediate child elements
getChildElements : Document a -> List $ Document ELEMENT
getChildElements (Element _ _ ns) = getElements ns
getChildElements _                = Nil

-- --------------------------------------------------------- [ Element Queries ]

private
getEElementsBy : (QName -> QName -> Bool)
              -> QName
              -> Document ELEMENT
              -> List $ Document ELEMENT
getEElementsBy eq qn e@(Element n as Nil) = if eq n qn then [e] else Nil
getEElementsBy eq qn e@(Element n as cs)  =
      if eq n qn
         then [e] ++ rest e
         else rest e
    where
      %assert_total
      rest : Document ELEMENT -> List (Document ELEMENT)
      rest (Element _ _ Nil) = Nil
      rest (Element _ _ cs)  = foldl (\ns,i => getEElementsBy eq qn i ++ ns) Nil (getElements cs)

private
getDocElementsBy : (QName -> QName -> Bool)
              -> QName
              -> Document DOCUMENT
              -> List $ Document ELEMENT
getDocElementsBy eq qn (MkDocument _ _ _ _ r) = getEElementsBy eq qn r


||| Get all Elements with a name.
getElementsBy : (QName -> QName -> Bool)
              -> QName
              -> Document a
              -> List $ Document ELEMENT
getElementsBy {a=DOCUMENT} eq qn d = getDocElementsBy eq qn d
getElementsBy {a=ELEMENT}  eq qn e = getEElementsBy eq qn e
getElementsBy              eq qn _ = Nil

||| Get all Elements with a given QName
getElementsByQName : QName  -> Document a -> List $ Document ELEMENT
getElementsByQName qn d = getElementsBy (==) qn d


||| Get all Elements with a given name. This ignores prefixes and namespaces.
getElementsByName : String -> Document a -> List $ Document ELEMENT
getElementsByName naam e = getElementsBy (\x,y => name x == name y) (mkQName naam) e

||| Get All Child Elements with a given QName.
getChildElementsByQName : QName -> Document a -> List $ Document ELEMENT
getChildElementsByQName qn n = filter (\x => getTag x == qn) (getChildElements n)

||| Get All Child Elements with a local name
getChildElementsByName : String -> Document a -> List $ Document ELEMENT
getChildElementsByName name n = getChildElementsByQName (mkQName name) n

||| Get All child elements regardless of name.
getAllChildren : Document a -> List $ Document ELEMENT
getAllChildren = getElementsBy (\x,y => True) (mkQName "empty")

-- --------------------------------------------------------------------- [ EOF ]
