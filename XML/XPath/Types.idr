--- --------------------------------------------------------------- [ Types.idr ]
-- Module      : XML.XPath.Types
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- Types for an XPath EDSL.
-- --------------------------------------------------------------------- [ EOH ]
module XML.XPath.Types

import XML.DOM

%default total
%access public export

data XPathTy = NODE | PATH | QUERY | ROOT | TEST Bool

data ValidPath : (head : XPathTy) -> (tail : XPathTy) -> Type where
  AbsPathRoot      : ValidPath ROOT NODE
  AbsPath          : ValidPath ROOT PATH
  AbsPathEnd       : ValidPath ROOT (TEST b)
  ValidSubEnd      : ValidPath NODE (TEST b)
  ValidSubPath     : ValidPath NODE NODE
  ValidSubPathPath : ValidPath NODE PATH

infixl 2 </>
infixl 2 <//>

||| Add anynode for singular case '//node'
||| Attributes
||| Predicates
data XPath : NodeTy -> XPathTy -> Type where
  ||| An XPath Query
  Query : XPath ty a -> XPath ty QUERY

  Any  : XPath ELEMENT NODE
  Elem : String -> XPath ELEMENT NODE

  Attr    : String -> XPath TEXT (TEST True)
  Text    : XPath TEXT    (TEST False)
  Comment : XPath COMMENT (TEST False)
  CData   : XPath CDATA   (TEST False)

  Root  : XPath ELEMENT NODE -> XPath ELEMENT ROOT
  DRoot : XPath ELEMENT NODE -> XPath ELEMENT ROOT

  ||| An absolute path
  PathA : XPath ELEMENT a
       -> XPath tyB b
       -> (prf : ValidPath a b)
       -> XPath tyB PATH
  ||| Get decendants
  PathD : XPath ELEMENT a
       -> XPath tyB b
       -> (prf : ValidPath a b)
       -> XPath tyB PATH

(</>) : XPath ELEMENT a
     -> XPath tyB b
     -> {auto prf : ValidPath a b}
     -> XPath tyB PATH
(</>) a b {prf} = PathA a b prf


(<//>) : XPath ELEMENT a
      -> XPath tyB b
      -> {auto prf : ValidPath a b}
      -> XPath tyB PATH
(<//>) a b {prf} = PathD a b prf

Show (XPath ty x) where
  show (Query q) = unwords ["[Query ", show q, "]\n"]
  show (Elem e)  = e
  show (Any)     = "*"
  show (Root r)  = "/" ++ show r
  show (DRoot r) = "//" ++ show r
  show (Attr a)  = "@" ++ a
  show (Text)    = "text()"
  show (Comment) = "comment()"
  show (CData)   = "cdata()"
  show (PathA p c prf) = show p ++ "/" ++ show c
  show (PathD p c prf) = show p ++ "//" ++ show c

-- ------------------------------------------------------------------- [ ERROR ]

data XPathError : Type where
  MalformedQuery : (qstr : String) -> (msg : String) -> XPathError
  QueryError     : (qstr : XPath ty a) -> (loc : XMLElem) -> (msg : Maybe String) -> XPathError
  SingletonError : String -> XPathError
  GenericError   : String -> XPathError

Show XPathError where
  show (MalformedQuery q err) = unwords
    [ "Query:"
    , show q
    , "is malformed because"
    , err]

  show (QueryError qstr loc msg) = unlines
    [ unwords ["QueryError:", fromMaybe "" msg]
    , "Asking for:"
    , unwords ["\t", show qstr]
    , "in"
    , unwords ["\t", (getTagName loc)]]
  show (GenericError msg) = unwords ["Generic Error:", msg]
  show (SingletonError m) = unwords ["At Least one node expected.", show m]
-- --------------------------------------------------------------------- [ EOF ]
