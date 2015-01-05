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

createDocType : String -> Maybe ExternalID -> DocType
createDocType name id = MkDocType name id

-- ------------------------------------------------------------- [ DOM Objects ]

-- Add predefined qnames with namepsaces

||| Creates a Document with a empty node
createDocument : QName -> Maybe DocType -> Document
createDocument n dtd = MkDoc prolog e Nil
  where
    e = MkElement n Nil Nil
    prolog = MkPrologue (createXMLNodeDefault) Nil dtd Nil

setRoot : Element -> Document -> Document
setRoot r doc = record {root = r} doc

createEmptyDoc : Document
createEmptyDoc = MkDoc prolog e Nil
  where
    e = MkElement (MkQName "empty" Nothing Nothing) Nil Nil
    prolog = MkPrologue (createXMLNodeDefault) Nil Nothing Nil

-- ------------------------------------------------------------------ [ QNames ]

||| Create a local Qualified Name
createQName : String -> QName
createQName n = MkQName n Nothing Nothing

||| Create a qualified name with a name space
createQNameNS : String -> String -> QName
createQNameNS n ns = MkQName n (Just ns) Nothing

||| Create a tag with namspace and prefix.
createTagNSPrefix : String -> String -> String -> QName
createTagNSPrefix n pre ns = MkQName n (Just ns) (Just pre)

||| Create a prefixed qualified name, intended for use with
||| attributes.
createAttrNamePrefix : String -> String -> QName
createAttrNamePrefix n pre = MkQName n Nothing (Just pre)

-- ---------------------------------------------------------------- [ Elements ]

||| Create a element with a local qualified name.
createSimpleElement : String -> Element
createSimpleElement name = MkElement tag Nil Nil
  where
   tag = createQName name

||| Create a element with a qualified name.
createElement : QName -> Element
createElement qname = MkElement qname Nil Nil

||| Create a element with a Namespace.
createElementNS : QName -> String -> Element
createElementNS qname ns = MkElement tag Nil Nil
  where
    tag = createQNameNS (name qname) ns

||| Create a key value pair
createAttribute : String -> String -> (QName, String)
createAttribute k v = (createQName k, v)

-- ----------------------------------------------------------- [ Node Creation ]

||| Create an XML Comment
createComment : String -> Node
createComment txt = NodeComment txt

createCData : String -> Node
createCData txt = NodeCData txt

createTextNode : String -> Node
createTextNode txt = NodeText txt

createProcessInstr : String -> String -> Node
createProcessInstr t d = NodeInstruction $ MkInstruction t d

createElementNode : String -> Node
createElementNode s = NodeElement $ createSimpleElement s
-- -------------------------------------------------------------- [ Attributes ]

getAttrName : (QName, String) -> String
getAttrName (k,v) = name k

getAttrPrefix : (QName, String) -> Maybe String
getAttrPrefix (k,v) = nprefix k

getAttrValue : (QName, String) -> String
getAttrValue (k,v) = v

-- ------------------------------------------------------------- [ Element Ops ]

class XMLNode a where
  ||| Get the attributes of the node
  getAttributes : a -> List (QName, String)

  ||| Does node have attributes
  hasAttributes : a -> Bool
  hasAttributes n = isCons (getAttributes n)

  ||| Get the children
  getNodes : a -> List Node

  ||| Does element have child nodes
  hasNodes : a -> Bool
  hasNodes n = isCons $ getNodes n

  ||| Add child to element
  appendChild : Node -> a -> a

  ||| Remove Child
  removeChild : Node -> a -> a

  ||| Get node name
  getNodeName : a -> String

  ||| Return the lement's value
  getValue : a -> Maybe String

  ||| Get tag name
  getTagName    : a -> Maybe String

  ||| Return an element's prefix
  getTagPrefix  : a -> Maybe String

  ||| Return an element's namespace
  getTagNS      : a -> Maybe String

-- ---------------------------------------------------------------- [ Elements ]
instance XMLNode Element where
  getAttributes e = attrs e
  getNodes      e = nodes e

  appendChild c e = record {nodes = c :: (nodes e) } e
  removeChild c e = record {nodes = delete c (nodes e)} e

  getTagName   e = Just $ name (tag e)
  getNodeName  e = name (tag e)
  getTagNS     e = nspace (tag e)
  getTagPrefix e = nprefix (tag e)
  getValue     e = Nothing

||| Get value for a given attribute
getAttribute : String -> Element -> Maybe String
getAttribute key e = lookup (createQName key) (attrs e)

||| Remove first occurance of attribute.
removeAttribute : String -> Element -> Element
removeAttribute key e = record { attrs = attrs'} e
  where
    attrs' = deleteBy (\(x,y), (a,b) => x==a )
                      (createQName key, "")
                      (attrs e)

||| Set first occurance of atttribute to new value.
setAttribute : (key : String) -> (value : String) -> Element -> Element
setAttribute k v e = record {attrs = attrs'} e
  where
    attrs' = createAttribute k v :: attrs (removeAttribute k e)

-- ------------------------------------------------------------------- [ Nodes ]

nodeGetAttrs : Node -> List (QName, String)
nodeGetAttrs (NodeElement e) = getAttributes e
nodeGetAttrs _               = Nil

nodeGetNodes : Node -> List Node
nodeGetNodes (NodeElement e) = getNodes e
nodeGetNodes _               = Nil

nodeAppendChild : Node -> Node -> Node
nodeAppendChild c (NodeElement e) = NodeElement $ appendChild c e
nodeAppendChild c e               = e

nodeRemoveChild : Node -> Node -> Node
nodeRemoveChild c (NodeElement e) = NodeElement $ removeChild c e
nodeRemoveChild c e               = e

-- http://docs.oracle.com/javase/7/docs/api/org/w3c/dom/Node.html#getNodeValue()
nodeGetName : Node -> String
nodeGetName (NodeCData       _) = "#cdata-section"
nodeGetName (NodeComment     _) = "#comment"
nodeGetName (NodeInstruction i) = iTarget i
nodeGetName (NodeText        _) = "#text"
nodeGetName (NodeElement     e) = getNodeName e

nodeGetValue : Node -> Maybe String
nodeGetValue (NodeCData       d) = Just d
nodeGetValue (NodeComment     c) = Just c
nodeGetValue (NodeInstruction i) = Just $ iData i
nodeGetValue (NodeText        t) = Just t
nodeGetValue (NodeElement     e) = Nothing

nodeGetTagName : Node -> Maybe String
nodeGetTagName (NodeElement e) = getTagName e
nodeGetTagName _               = Nothing

nodeGetTagPrefix : Node -> Maybe String
nodeGetTagPrefix (NodeElement e) = getTagPrefix e
nodeGetTagPrefix _               = Nothing

nodeGetTagNS : Node -> Maybe String
nodeGetTagNS (NodeElement e) = getTagNS e
nodeGetTagNS _               = Nothing

instance XMLNode Node where
  getAttributes = nodeGetAttrs
  getNodes      = nodeGetNodes

  appendChild = nodeAppendChild
  removeChild = nodeRemoveChild

  getNodeName = nodeGetName
  getValue    = nodeGetValue

  getTagName   = nodeGetTagName
  getTagNS     = nodeGetTagNS
  getTagPrefix = nodeGetTagPrefix

getElement : Node -> Maybe Element
getElement (NodeElement e) = Just e
getElement _               = Nothing

getChildElements : Node -> List Element
getChildElements (NodeElement e) = catMaybes $ map (getElement) (getNodes e)
getChildElements _               = Nil


||| Get all Elements with a name.
getElementsByQName : QName  -> Node -> List Element
getElementsByQName qn (NodeElement e) = if (tag e) == qn
    then [e] ++ concatMap (getNodeElem) (nodes e)
   else concatMap (getNodeElem) (nodes e)
  where
    getNodeElem : Node -> List Element
    getNodeElem (NodeElement e) = getElementsByQName qn (NodeElement e)
    getNodeElem _               = Nil
getElementsByQName _   _               = Nil

||| Get all Elements with a local name.
getElementsByName : String -> Node -> List Element
getElementsByName naam e = getElementsByQName (createQName naam) e

||| Get All Child Elements with a name
getChildElememtsByQName : QName -> Node -> List Element
getChildElememtsByQName qn n = filter (\x => tag x == qn) (getChildElements n)

||| Get All Child Elements with a local name
getChildElememtsByName : String -> Node -> List Element
getChildElememtsByName name n = getChildElememtsByQName (createQName name) n

-- --------------------------------------------------------------------- [ EOF ]
