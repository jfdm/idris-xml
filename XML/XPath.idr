-- --------------------------------------------------------------- [ XPath.idr ]
-- Module    : XPath.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module XML.XPath

import XML.DOM

import XML.XPath.Parser

%access export

public export
data XPathTy = NODE | PATH | QUERY | ROOT | TEST Bool

public export
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
public export
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

public export
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

public export
data XPathError : Type where
  MalformedQuery : (qstr : String) -> (msg : String) -> XPathError
  QueryError     : (qstr : XPath ty a) -> (loc : XMLElem) -> (msg : Maybe String) -> XPathError
  SingletonError : String -> XPathError
  GenericError   : String -> XPathError

export
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


-- ------------------------------------------------------------------ [ Parser ]
{-
private
doParse : String -> Either String (XPath ty QUERY)
doParse = Strings.parse parseQuery

public export
calcQueryType : String -> Type
calcQueryType str =
  case (doParse str) of
    Left err  => XPathError
    Right res => evalTy res

private
doQuery : (res : String)
       -> Document ELEMENT
       -> calcQueryType res
doQuery qstr e =
  case parse parseQuery qstr of
    Left err => Left  $ MalformedQuery qstr err
    Right q  => Right $ evaluatePath q e

-}
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
-- --------------------------------------------------------------------- [ EOF ]
