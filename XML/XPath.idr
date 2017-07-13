-- --------------------------------------------------------------- [ XPath.idr ]
-- Module    : XPath.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module XML.XPath

import XML.DOM

import public XML.XPath.Types
import XML.XPath.Parser

%access private

-- ------------------------------------------------------------------- [ ERROR ]

public export
data XPathError : Type where
  MalformedQuery : (qstr : String) -> (msg : String) -> XPathError
  QueryError     : (qstr : XPath a) -> (loc : XMLElem) -> (msg : Maybe String) -> XPathError
  SingletonError : String -> XPathError
  GenericError   : String -> XPathError

public export
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

evalTy : XPath a -> Type
evalTy (Query x) = evalTy x
evalTy Any = List $ Document ELEMENT
evalTy (Elem x) = List $ Document ELEMENT
evalTy (Attr x) = Maybe String
evalTy Text = List $ Document TEXT
evalTy Comment = List $ Document COMMENT
evalTy CData = List $ Document CDATA
evalTy (Root x) = evalTy x
evalTy (DRoot x) = evalTy x
evalTy (x </> y) = evalTy y
evalTy (x <//> y) = evalTy y

private
evaluatePath : (q : XPath a)
            -> Document ELEMENT
            -> evalTy q

evaluatePath (Query q) n = evaluatePath q n
evaluatePath (Elem e)  n = getChildElementsByName e n
evaluatePath (Any)     n = getChildElements n

evaluatePath (Attr a)  n = getAttribute a n

evaluatePath (CData)   n = getCData n
evaluatePath (Text)    n = getText n
evaluatePath (Comment) n = getComments n

evaluatePath (Root r)  n with (r)
    | Any    = [n]
    | Elem e = if getTagName n == e then [n] else Nil

evaluatePath (DRoot r) n with (r)
    | Any    = getAllChildren n
    | Elem e = getElementsByName e n

evaluatePath (p </> c) n = concatMap (evaluatePath c) $ getElements (evaluatePath p n)
evaluatePath (p <//> child) n with (child)
    | Any    = fromList $ concatMap (getAllChildren) $ getElements (evaluatePath p n)
    | Elem c = fromListList $ concatMap (getElementsByName c) $ getElements (evaluatePath p n)
    | path   = concatMap (evaluatePath path) $ getElements (evaluatePath p n)

-- ------------------------------------------------------------------ [ Parser ]

private
doQuery : String
       -> Document ELEMENT
       -> Either XPathError (List $ Document NODE)
doQuery qstr e =
  case parse parseQuery qstr of
    Left err => Left  $ MalformedQuery qstr err
    Right q  => Right $ evaluatePath q e

export
query : String
     -> Document ty
     -> {auto prf : CanQuery ty}
     -> Either XPathError (List $ Document NODE)
query {ty=ELEMENT}  qstr n                      = doQuery qstr n
query {ty=DOCUMENT} qstr (MkDocument _ _ _ _ e) = doQuery qstr e

-- --------------------------------------------------------------------- [ EOF ]
