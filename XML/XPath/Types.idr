--- --------------------------------------------------------------- [ Types.idr ]
-- Module      : XML.XPath.Types
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- Types for an XPath EDSL.
-- --------------------------------------------------------------------- [ EOH ]
module XML.XPath.Types


data XPathTy = NODE | PATH | QUERY | ROOT

-- Combine predicates
data ValidPathHead : XPathTy -> Type where
  ValidHeadNode : ValidPathHead NODE
  ValidHeadRoot : ValidPathHead ROOT

data ValidPathElem : XPathTy -> Type where
  ValidElem : ValidPathElem NODE
  ValidPath : ValidPathElem PATH

infixl 2 </>
||| Add anynode
||| Attributes
||| Predicates
data XPath : XPathTy -> Type where
  Query : XPath a -> XPath QUERY
  Elem : String -> XPath NODE
  Root : String -> XPath ROOT
  (</>) : XPath a
        -> XPath b
        -> {auto prf : ValidPathElem b}
        -> {auto prf1 : ValidPathHead a}
        -> XPath PATH

-- ---------------------------------------------------------------------- [ EO ]
