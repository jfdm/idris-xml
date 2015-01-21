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

||| Creates a Document with a empty node
mkDocument : QName -> Maybe DocType -> Document DOCUMENT
mkDocument n dtd = MkDocument (createXMLInfoDefault) dtd Nil Nothing
                              (Element n Nil Nil)

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
   tag = mkQName name

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
mkCommentNode : String -> Document COMMENT
mkCommentNode txt = Comment txt

mkCData : String -> Document CDATA
mkCData txt = CData txt

mkTextNode : String -> Document TEXT
mkTextNode txt = Text txt

mkInstructNode : String -> List (String, String) -> Document INSTRUCTION
mkInstructNode t d = Instruction t d

mkElementNode : String -> Document ELEMENT
mkElementNode s = mkSimpleElement s

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
appendToNode c (Element n as ns) prf = Element n as $ (::) {prf = prf} c ns

private
removeFromNode : Document a
               -> Document ELEMENT
               -> (prf : ValidNode a)
               -> Document ELEMENT
removeFromNode c (Element n as ns) prf = Element n as (delete {p=prf} c ns)

||| Set Value
addText : String -> Document ELEMENT -> Document ELEMENT
addText s e = appendToNode (mkTextNode s) e IsOK

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
(<=>) e s = e <++> (mkTextNode s)

||| Create and add text value
(<+=>) : String -> String -> Document ELEMENT
(<+=>) n v = (mkSimpleElement n) <=> v

-- --------------------------------------------------------------- [ Accessors ]
||| Get the attributes of the node
getAttributes : Document a -> List (QName, String)
getAttributes (Element _ as _) = as
getAttributes _                = Nil

||| Does node have attributes
hasAttributes : Document a -> Bool
hasAttributes n = isCons (getAttributes n)

||| Get the children
getNodes : Document a -> Document NODES
getNodes (Element _ _ ns) = ns
getNodes _                = Nil

||| Does element have child nodes
hasNodes : Document a -> Bool
hasNodes n = isCons $ getNodes n

||| Get node name
||| http://docs.oracle.com/javase/7/docs/api/org/w3c/dom/Node.html
getNodeName : Document a -> Maybe String
getNodeName (CData _)          = Just $ "#cdata-section"
getNodeName (Comment  _)       = Just $ "#comment"
getNodeName (Instruction i _)  = Just $ i
getNodeName (Text _)           = Just $ "#text"
getNodeName (Element naam _ _) = Just $ name naam
getNodeName _                  = Nothing

||| Return the element's value
getNodeValue : Document a -> Maybe String
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
    attrs' = deleteBy (\(x,y), (a,b) => name x== name a )
                      (mkQName key, "")
                      (as)

||| Set first occurance of atttribute to new value.
setAttribute : (key : String)
             -> (value : String)
             -> Document ELEMENT
             -> Document ELEMENT
setAttribute k v (Element n as ns) = Element n attrs' ns
  where
    attrs' = mkAttribute k v :: (deleteBy (\(x,y), (a,b) => name x== name a )
                                          (mkQName k, "") (as))



-- ------------------------------------------------------------ [ Node Queries ]
{-- Attempts at making dry.
getElementsByType : {ty : NodeTy} -> NodeTy -> Document NODES -> List $ Document ty
getElementsByType _  Nil     = Nil
getElementsByType ty (x::xs) = if getDocElemTy x == ty
    then x :: getElementsByType ty xs
    else getElementsByType ty xs

getChildrenBy : {ty : NodeTy} -> NodeTy -> Document a -> List $ Document ty
getChildrenBy ty (Element _ _ ns) = getElementsByType ty ns
getChildrenBy _  _                = Nil
-}

||| getElements
getElements : Document NODES -> List $ Document ELEMENT
getElements Nil     = Nil
getElements (x::xs) with (x)
    | (Element _ _ _) = x :: getElements xs
    | otherwise       = getElements xs

getText : Document NODES -> List $ Document TEXT
getText Nil = Nil
getText (x::xs) with (x)
    | (Text t)  = x :: getText xs
    | otherwise = getText xs

getComments : Document NODES -> List $ Document COMMENT
getComments Nil = Nil
getComments (x::xs) with (x)
    | (Comment t)  = x :: getComments xs
    | otherwise = getComments xs

getCData : Document NODES -> List $ Document CDATA
getCData Nil = Nil
getCData (x::xs) with (x)
    | (CData t)  = x :: getCData xs
    | otherwise = getCData xs


||| Get the immediate child elements
getChildElements : Document a -> List $ Document ELEMENT
getChildElements (Element _ _ ns) = getElements ns
getChildElements _                = Nil

-- --------------------------------------------------------- [ Element Queries ]
mutual
  private
  func : (QName -> QName -> Bool)
       -> QName
       -> List $ Document ELEMENT
       -> List $ Document ELEMENT
  func _ _ Nil     = Nil
  func f n (x::xs) = getElementsBy f n x ++ func f n xs

  ||| Get all Elements with a name.
  getElementsBy : (QName -> QName -> Bool)
                -> QName
                -> Document a
                -> List $ Document ELEMENT
  getElementsBy eq qn e with (e)
     | (MkDocument _ _ _ _ r) = getElementsBy eq qn r
     | (Element _ _ _) = if eq (getTag e) qn
         then [e] ++ func eq qn (getChildElements e)
         else func eq qn (getChildElements e)
     | otherwise = Nil

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
