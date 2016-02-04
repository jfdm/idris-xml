--- --------------------------------------------------------------- [ Types.idr ]
-- Module      : XML.XPath.Types
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- Types for an XPath EDSL.
-- --------------------------------------------------------------------- [ EOH ]
module XML.XPath.Types

%access public export

data XPathTy = NODE | PATH | QUERY | ROOT | TEST

data ValidPath : (head : XPathTy) -> (tail : XPathTy) -> Type where
  AbsPathRoot      : ValidPath ROOT NODE
  AbsPath          : ValidPath ROOT PATH
  AbsPathEnd       : ValidPath ROOT TEST
  ValidSubEnd      : ValidPath NODE TEST
  ValidSubPath     : ValidPath NODE NODE
  ValidSubPathPath : ValidPath NODE PATH

infixl 2 </>
infixl 2 <//>

||| Add anynode for singular case '//node'
||| Attributes
||| Predicates
data XPath : XPathTy -> Type where
  ||| An XPath Query
  Query : XPath a -> XPath QUERY

  Any  : XPath NODE
  Elem : String -> XPath NODE

  Attr    : String -> XPath TEST
  Text    : XPath TEST
  Comment : XPath TEST
  CData   : XPath TEST

  Root  : XPath NODE -> XPath ROOT
  DRoot : XPath NODE -> XPath ROOT

  ||| An absolute path
  (</>) : XPath a
        -> XPath b
        -> {auto prf : ValidPath a b}
        -> XPath PATH
  ||| Get decendants
  (<//>) : XPath a
         -> XPath b
         -> {auto prf : ValidPath a b}
         -> XPath PATH

public export
Show (XPath x) where
  show (Query q) = unwords ["[Query ", show q, "]\n"]
  show (Elem e)  = e
  show (Any)     = "*"
  show (Root r)  = "/" ++ show r
  show (DRoot r) = "//" ++ show r
  show (Attr a)  = "@" ++ a
  show (Text)    = "text()"
  show (Comment) = "comment()"
  show (CData)   = "cdata()"
  show (p </> c) = show p ++ "/" ++ show c
  show (p <//> c) = show p ++ "//" ++ show c

-- --------------------------------------------------------------------- [ EOF ]
