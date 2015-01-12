--- --------------------------------------------------------------- [ Types.idr ]
-- Module      : XML.XPath.Types
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- Types for an XPath EDSL.
-- --------------------------------------------------------------------- [ EOH ]
module XML.XPath.Types

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
  ||| Find a node.
  Elem : String -> XPath NODE
  ||| Find an attribute
  Attr : String -> XPath TEST
  ||| Node Test
  Text : XPath TEST
  Comment : XPath TEST
  ||| Get Root
  Root : String -> XPath ROOT
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

instance Show (XPath x) where
  show (Query q) = unwords ["[Query ", show q, "]\n"]
  show (Elem e)  = e
  show (Root r)  = "/" ++ r
  show (Attr a)  = "@" ++ a
  show (Text)    = "text()"
  show (Comment) = "comment()"
  show (p </> c) = show p ++ "/" ++ show c
  show (p <//> c) = show p ++ "//" ++ show c

-- --------------------------------------------------------------------- [ EOF ]
