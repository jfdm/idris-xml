-- ----------------------------------------------------------------- [ DOM.idr ]
-- Module      :
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- A DOM Style API to XML data inspired by
-- `https://docs.python.org/2/library/xml.dom.html`
-- --------------------------------------------------------------------- [ EOH ]
module XML.DOM

import XML.Types
import public XML.Utils

%access public

-- ---------------------------------------------------------------- [ DocTypes ]

-- Add predefined doctypes for xhtml html html5 xml

mkDocType : String -> Maybe ExternalID -> DocType
mkDocType name id = MkDocType name id

-- ------------------------------------------------------------- [ DOM Objects ]

-- Add predefined qnames with namepsaces

||| Creates a Document with a empty node
mkDocument : QName -> Maybe DocType -> Document DOCUMENT
mkDocument n dtd = MkDocument (createXMLInfoDefault) dtd Nil Nothing
                              (Element n Nil Nil)

-- ------------------------------------------------------------------ [ QNames ]

||| Create a local Qualified Name
mkQName : String -> QName
mkQName n = MkQName n Nothing Nothing

||| Create a qualified name with a name space
mkQNameNS : String -> String -> QName
mkQNameNS n ns = MkQName n (Just ns) Nothing

||| Create a tag with namspace and prefix.
mkTagNSPrefix : String -> String -> String -> QName
mkTagNSPrefix n pre ns = MkQName n (Just ns) (Just pre)

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
mkElementNS : QName -> String -> Document ELEMENT
mkElementNS qname ns = Element tag Nil Nil
  where
    tag = mkQNameNS (name qname) ns

||| Create a key value pair
mkAttribute : String -> String -> (QName, String)
mkAttribute k v = (mkQName k, v)

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

||| Add child to an element node
appendToNode : Document a
           -> Document ELEMENT
           -> {auto prf : ValidNode a}
           -> Document ELEMENT
appendToNode c (Element n as ns) = Element n as (c :: ns)


||| Remove Child from Element
removeFromNode : Document a
               -> Document ELEMENT
               -> {auto prf : ValidNode a}
               -> Document ELEMENT
removeFromNode c (Element n as ns) = Element n as (delete c ns)

||| Set Value
addText : String -> Document ELEMENT -> Document ELEMENT
addText s e = appendToNode (mkTextNode s) e

--  -------------------------------------------------------------------- [ Ops ]

||| Append
(<++>) : Document ELEMENT -> Document a -> {auto prf : ValidNode a} -> Document ELEMENT
(<++>) p c = appendToNode c p

||| Remove
(<-->) : Document ELEMENT -> Document a -> {auto prf : ValidNode a} -> Document ELEMENT
(<-->) p c = removeFromNode c p

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
getAttribute key e = lookup (mkQName key) (getAttributes e)

||| Remove first occurance of attribute.
removeAttribute : String -> Document ELEMENT -> Document ELEMENT
removeAttribute key (Element n as ns) = Element n (attrs') ns
  where
    attrs' = deleteBy (\(x,y), (a,b) => x==a )
                      (mkQName key, "")
                      (as)

||| Set first occurance of atttribute to new value.
setAttribute : (key : String) -> (value : String) -> Document ELEMENT -> Document ELEMENT
setAttribute k v (Element n as ns) = Element n attrs' ns
  where
    attrs' = mkAttribute k v :: (deleteBy (\(x,y), (a,b) => x==a ) (mkQName k, "") (as))

||| getElements
getElements : Document NODES -> List $ Document ELEMENT
getElements Nil     = Nil
getElements (x::xs) with (x)
    | (Element _ _ _) = x :: getElements xs
    | otherwise       = getElements xs

||| Get the immediate child elements
getChildElements : Document a -> List $ Document ELEMENT
getChildElements (Element _ _ ns) = getElements ns
getChildElements _                = Nil

||| Get all Elements with a name.
getElementsByQName : QName  -> Document a -> List $ Document ELEMENT
getElementsByQName qn (Element n as ns) = if n == qn
    then [Element n as ns] ++ func qn ns
    else func qn ns
  where
    func : QName -> Document NODES -> List $ Document ELEMENT
    func _ Nil     = Nil
    func n (x::xs) = getElementsByQName n x ++ func n xs

getElementsByQName _   _ = Nil

||| Get all Elements with a local name.
getElementsByName : String -> Document a -> List $ Document ELEMENT
getElementsByName naam e = getElementsByQName (mkQName naam) e


||| Get All Child Elements with a name
getChildElementsByQName : QName -> Document a -> List $ Document ELEMENT
getChildElementsByQName qn n = filter (\x => getTag x == qn) (getChildElements n)

||| Get All Child Elements with a local name
getChildElementsByName : String -> Document a -> List $ Document ELEMENT
getChildElementsByName name n = getChildElementsByQName (mkQName name) n

-- --------------------------------------------------------------------- [ EOF ]
