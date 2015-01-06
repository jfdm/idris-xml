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
mkCommentNode : String -> CommentNode
mkCommentNode txt = mkNode $ Comment txt

mkCData : String -> CDataNode
mkCData txt = mkNode $ CData txt

mkTextNode : String -> TextNode
mkTextNode txt = mkNode $ Text txt

mkInstructNode : String -> List (String, String) -> InstructionNode
mkInstructNode t d = mkNode $ Instruction t d

mkElementNode : String -> ElementNode
mkElementNode s = mkNode $ mkSimpleElement s

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

class NodeOps e where
  (<++>) : e -> (a : NodeTy ** Document a) -> {auto prf : ValidNode a} -> e
  (<-->) : e -> (a : NodeTy ** Document a) -> {auto prf : ValidNode a} -> e
  (<=>)  : e -> String -> e
  (<+=>) : String -> String -> e

||| Add child to element
appendToElem : (a : NodeTy ** Document a)
             -> Document ELEMENT
             -> {auto prf : ValidNode a}
             -> Document ELEMENT
appendToElem c (Element n as ns) = Element n as (c :: ns)

||| Add child to an element node
appendToNode : (a : NodeTy ** Document a)
           -> ElementNode
           -> {auto prf : ValidNode a}
           -> ElementNode
appendToNode c (ELEMENT ** p) = mkNode $ appendToElem c p


||| Remove Child from Element
removeFromElem : (a : NodeTy ** Document a)
               -> Document ELEMENT
               -> {auto prf : ValidNode a}
               -> Document ELEMENT
removeFromElem c (Element n as ns) = Element n as (deleteBy nDel c ns)
  where
    nDel : (a : NodeTy ** Document a)
         -> (b : NodeTy ** Document b)
         -> Bool
    nDel a b = getWitness a == getWitness b

||| Remove Child from Node
removeFromNode : (a : NodeTy ** Document a)
               -> ElementNode
               -> {auto prf : ValidNode a}
               -> ElementNode
removeFromNode c (ELEMENT ** p) = mkNode $ removeFromElem c p

||| Set Value
addText : String -> ElementNode -> ElementNode
addText s e = appendToNode (mkTextNode s) e

--  -------------------------------------------------------------------- [ Ops ]

instance NodeOps (Document ELEMENT) where
  (<++>) p c = appendToElem c p
  (<-->) p c = removeFromElem c p
  (<=>) e s = e <++> (mkTextNode s)
  (<+=>) n v = (mkSimpleElement n) <=> v

instance NodeOps ElementNode where
  (<++>) p c = appendToNode c p
  (<-->) p c = removeFromNode c p
  (<=>) e s = e <++> (mkTextNode s)
  (<+=>) name value = (mkElementNode name) <=> value


-- --------------------------------------------------------------- [ Accessors ]
||| Get the attributes of the node
getAttributes : Document a -> List (QName, String)
getAttributes (Element _ as _) = as
getAttributes _                = Nil

||| Does node have attributes
hasAttributes : Document a -> Bool
hasAttributes n = isCons (getAttributes n)

||| Get the children
getNodes : Document a -> NodeList
getNodes (Element _ _ ns) = ns
getNodes _                = Nil

||| Does element have child nodes
hasNodes : Document a -> Bool
hasNodes n = isCons $ getNodes n

||| Get node name
||| http://docs.oracle.com/javase/7/docs/api/org/w3c/dom/Node.html
getNodeName : ( a : NodeTy ** Document a) -> String
getNodeName (_ ** (CData       _))    = "#cdata-section"
getNodeName (_ ** (Comment     _))    = "#comment"
getNodeName (_ ** (Instruction i _))  = i
getNodeName (_ ** (Text        _))    = "#text"
getNodeName (_ ** (Element naam _ _)) = name naam

||| Return the lement's value
getNodeValue : (a : NodeTy ** Document a) -> Maybe String
getNodeValue (_ ** (CData       d))   = Just d
getNodeValue (_ ** (Comment     c))   = Just c
getNodeValue (_ ** (Instruction _ d)) = Just $ concatMap show d
getNodeValue (_ ** (Text        t))   = Just t
getNodeValue (_ ** (Element _ _ _))   = Nothing

getTag : Document ELEMENT -> QName
getTag (Element n _ _) = n

||| Get tag name
getNodeTag : ElementNode -> QName
getNodeTag (_ ** (Element n _ _)) = n

||| Get an elements tag name
getElementName : Document ELEMENT -> String
getElementName (Element n _ _) = name n

||| Get tag name
getTagName : ElementNode -> String
getTagName (_ ** (Element n _ _)) = name n

||| Return an element's prefix
getElementPrefix : Document ELEMENT -> Maybe String
getElementPrefix (Element n _ _) = nprefix n

||| Return an element's prefix
getTagPrefix : ElementNode -> Maybe String
getTagPrefix (_ ** (Element n _ _)) = nprefix n

||| Return an element's namespace
getElementNS : Document ELEMENT -> Maybe String
getElementNS (Element n _ _) = nspace n

||| Return an element's namespace
getTagNS : ElementNode -> Maybe String
getTagNS (_ ** (Element n _ _ )) = nspace n

||| Get value for a given attribute
getAttribute : String -> Document ELEMENT -> Maybe String
getAttribute key e = lookup (mkQName key) (getAttributes e)

getNodeAttribute : String -> ElementNode -> Maybe String
getNodeAttribute key (_ ** (Element _ as _)) = lookup (mkQName key) as

||| Remove first occurance of attribute.
removeAttribute : String -> Document ELEMENT -> Document ELEMENT
removeAttribute key (Element n as ns) = Element n (attrs') ns
  where
    attrs' = deleteBy (\(x,y), (a,b) => x==a )
                      (mkQName key, "")
                      (as)

removeNodeAttribute : String -> ElementNode -> ElementNode
removeNodeAttribute key (_ ** Element n as ns) = mkNode $ Element n as' ns
  where
    as' = deleteBy (\(x,y), (a,b) => x==a )
                   (mkQName key, "")
                   (as)

||| Set first occurance of atttribute to new value.
setAttribute : (key : String) -> (value : String) -> Document ELEMENT -> Document ELEMENT
setAttribute k v (Element n as ns) = Element n attrs' ns
  where
    attrs' = mkAttribute k v :: (deleteBy (\(x,y), (a,b) => x==a ) (mkQName k, "") (as))

setNodeAttribute : String -> String -> ElementNode -> ElementNode
setNodeAttribute k v (_ ** Element n as ns) = mkNode $ Element n as' ns
  where
      as' = mkAttribute k v :: (deleteBy (\(x,y), (a,b) => x==a ) (mkQName k, "") (as))

||| Get the immediate child elements nodes
getChildElementNodes : ElementNode -> List ElementNode
getChildElementNodes (_ ** Element _ _ ns) = filter (\x => getWitness x == ELEMENT) ns
getChildElementNodes _                     = Nil

||| Get the immediate child elements
getChildElements : ElementNode -> List $ Document ELEMENT
getChildElements (_ ** (Element _ _ ns)) = catMaybes $ map getES ns
  where
    getES : (a : NodeTy ** Document a) -> Maybe $ Document ELEMENT
    getES (ELEMENT ** p) = Just p
    getES _              = Nothing
getChildElements _ = Nil

||| Get all Elements with a name.
getElementsByQName : QName  -> ElementNode -> List $ Document ELEMENT
getElementsByQName qn (_ ** Element n as ns) = if n == qn
    then [Element n as ns] ++ concatMap (getNodeElem) ns
   else concatMap (getNodeElem) ns
  where
    getNodeElem : (a : NodeTy ** Document a) -> List $ Document ELEMENT
    getNodeElem n with (n)
       | (ELEMENT ** e) = getElementsByQName qn n
       | otherwise      = Nil
getElementsByQName _   _               = Nil

||| Get all Elements with a local name.
getElementsByName : String -> (a : NodeTy ** Document a) -> List $ Document ELEMENT
getElementsByName naam e = getElementsByQName (mkQName naam) e

||| Get All Child Elements with a name
getChildElementsByQName : QName -> (a : NodeTy ** Document a) -> List $ Document ELEMENT
getChildElementsByQName qn n = filter (\x => getTag x == qn) (getChildElements n)

||| Get All Child Elements with a local name
getChildElementsByName : String -> (a : NodeTy ** Document a) -> List $ Document ELEMENT
getChildElementsByName name n = getChildElementsByQName (mkQName name) n

getElement : (a : NodeTy ** Document a) -> Maybe $ Document ELEMENT
getElement (ELEMENT ** p) = Just p
getElement _              = Nothing
-- --------------------------------------------------------------------- [ EOF ]
