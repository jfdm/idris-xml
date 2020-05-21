-- --------------------------------------------------------------- [ XPath.idr ]
-- Module    : XPath.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module XML.XPath

import XML.DOM

import public XML.XPath.Types
import XML.XPath.Parser

%access export

-- ------------------------------------------------------------------- [ Query ]

public export
data XPathResultTy = NODELIST | VALUE

public export
data XPathResult : NodeTy -> Type where
  Nodes : (prf : ValidNode a)
       -> List (Document a)
       -> XPathResult a
  Empty  : XPathResult a

foldResults : List (XPathResult a) -> XPathResult a
foldResults = foldl merge Empty
  where
    merge : XPathResult a -> XPathResult a -> XPathResult a
    merge Empty y     = y
    merge x     Empty = x
    merge (Nodes prf xs) (Nodes x ys) = Nodes prf (xs ++ ys)

concatMapResults : (Document ELEMENT -> XPathResult ty) -> XPathResult ELEMENT -> XPathResult ty
concatMapResults f (Nodes prf xs) = foldResults $ Functor.map f xs
concatMapResults f Empty = Empty

private
evaluatePath : (q : XPath ty a)
            -> Document ELEMENT
            -> XPathResult ty -- (calcResultTy q)

evaluatePath (Query q) n = evaluatePath q n
evaluatePath (Elem e)  n = Nodes ValidElem $ getChildElementsByName e n
evaluatePath (Any)     n = Nodes ValidElem $ getChildElements n

evaluatePath (Attr a)  n with (getAttribute a n)
  evaluatePath (Attr a)  n | Nothing = Empty
  evaluatePath (Attr a)  n | (Just x) = Nodes ValidText [(mkText x)]


evaluatePath (CData)   n = Nodes ValidCData $ getCData n
evaluatePath (Text)    n = Nodes ValidText  $ getText n
evaluatePath (Comment) n = Nodes ValidDoc   $ getComments n

evaluatePath (Root r)  n with (r)
    | Any    = Nodes ValidElem [n]
    | Elem e = if getTagName n == e then Nodes ValidElem [n] else Empty

evaluatePath (DRoot r) n with (r)
    | Any    = Nodes ValidElem $ getAllChildren n
    | Elem e = Nodes ValidElem $ getElementsByName e n

evaluatePath (PathA p child prf {a}) n = concatMapResults (evaluatePath child) (evaluatePath p n)

evaluatePath (PathD p child prf) n with (child)
  evaluatePath (PathD p child prf) n | Any with (evaluatePath p n)
    evaluatePath (PathD p child prf) n | Any | (Nodes x xs) =
        Nodes ValidElem $ Foldable.concatMap (\x => getAllChildren x) xs
    evaluatePath (PathD p child prf) n | Any | Empty = Empty

  evaluatePath (PathD p child prf) n | (Elem x) with (evaluatePath p n)
    evaluatePath (PathD p child prf) n | (Elem x) | (Nodes ValidElem xs) =
        Nodes ValidElem $ Foldable.concatMap (\e => getElementsByName x e) xs
    evaluatePath (PathD p child prf) n | (Elem x) | Empty = Empty

  evaluatePath (PathD p child prf) n | path with (evaluatePath p n)
    evaluatePath (PathD p child prf) n | path | res =
        concatMapResults (evaluatePath path) res


-- ------------------------------------------------------------------- [ Query ]

export
queryDoc : XPath ty QUERY
        -> Document DOCUMENT
        -> XPathResult ty
queryDoc qstr doc = evaluatePath qstr (getRoot doc)

export
queryElem : XPath ty QUERY
         -> Document ELEMENT
         -> XPathResult ty
queryElem qstr e = evaluatePath qstr e

export
query : XPath qTy QUERY
     -> Document ty
     -> {auto prf : CanQuery ty}
     -> XPathResult qTy
query q x {prf} with (prf)
  query q x {prf = prf} | QueryDoc = queryDoc q x
  query q x {prf = prf} | QueryElem = queryElem q x


-- ------------------------------------------------------------------ [ Parser ]

namespace String
  export
  queryDoc : String
          -> Document DOCUMENT
          -> Either XPathError (ty ** XPathResult ty)
  queryDoc qstr doc =
    case (parseXPathStr qstr) of
      Left err        => Left $ MalformedQuery qstr err
      Right (_ ** q)  => Right $ (_ ** queryDoc q doc)

  export
  queryElem : String
           -> Document ELEMENT
           -> Either XPathError (ty ** XPathResult ty)
  queryElem qstr e = do
    case (parseXPathStr qstr) of
      Left err        => Left $ MalformedQuery qstr err
      Right (_ ** q)  => Right $ (_ ** queryElem q e)

  export
  query : String
       -> Document ty
       -> {auto prf : CanQuery ty}
       -> Either XPathError (ty' ** XPathResult ty')
  query qstr x {prf} with (prf)
    query qstr x {prf = prf} | QueryDoc = queryDoc qstr x
    query qstr x {prf = prf} | QueryElem = queryElem qstr x


-- --------------------------------------------------------------------- [ EOF ]
