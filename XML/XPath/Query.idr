-- --------------------------------------------------------------- [ Query.idr ]
-- Module    : Query.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Predefined Query Wrappers for XPath stuff.
module XML.XPath.Query

import XML.DOM
import XML.XPath

%access export

||| Use XPath to find some nodes satisfying some query
|||
||| @convErr Convert XPathError to a local Error Type
||| @qstr    The query to run.
||| @node    The thing we are querying
getNodes : (convErr : XPathError -> a)
        -> (qstr : String)
        -> (node : Document ty)
        -> {auto prf : CanQuery ty}
        -> Either a (ty' ** XPathResult ty')
getNodes convErr qstr doc =
  case query qstr doc of
    Left err  => Left $ convErr err
    Right res => Right res

||| Use XPath to find the first node that satisfies some query.
|||
||| @convErr Convert XPathError to a local Error Type
||| @qstr    The query to run.
||| @node    The thing we are querying
getNode : (convErr : XPathError -> a)
       -> (qstr : String)
       -> (node : Document ty)
       -> {auto prf : CanQuery ty}
       -> Either a (ty' ** Document ty')
getNode convErr qstr doc = do
  (_ ** res) <- getNodes convErr qstr doc
  case res of -- QueryError qstr loc msg
    Empty    => Left (convErr $ SingletonError qstr)
    (Nodes _ (x::_)) => Right (_ ** x)


||| Use XPath to find nodes satisfying some query get the text or
||| cdata inside or a specific attribute.
|||
||| @test    The test to execute.
||| @convErr Convert XPathError to a local error type.
||| @qstr    The query to run.
||| @node    The thing we are querying.
getNodeValues : (test : XPath ty' (TEST b))
             -> (convErr : XPathError -> a)
             -> (qstr : String)
             -> (node  : Document ty)
             -> {auto prfB : ValidNode ty}
             -> {auto prf : CanQuery ty}
             -> Either a (List String)
getNodeValues test convErr qstr doc = do
    (_ ** res) <- getNodes convErr (concat [qstr, "/", show test]) doc
    case res of
      Empty => pure Nil
      (Nodes prf xs) => do
        let vals = mapMaybe (\x => getNodeValue {prf=prf} x) xs
        pure vals

||| Use XPath to find the first node that satisfies some query get the
||| text or cdata inside or a specific attribute.
|||
||| @test    The test to execute.
||| @convErr Convert XPathError to a local error type.
||| @qstr    The query to run.
||| @node    The thing we are querying.
getNodeValue : (test : XPath ty' (TEST b))
            -> (convErr : XPathError -> a)
            -> (qstr : String)
            -> (node : Document ty)
            -> {auto prfV : ValidNode ty}
            -> {auto prf : CanQuery ty}
            -> Either a String
getNodeValue test convErr qstr doc = do
    res <- getNodeValues test convErr qstr doc
    case res of
      Nil    => Left (convErr $ SingletonError (qstr))
      (x::_) => Right x

||| Use XPath to get the values for a named attribute in a node satisfying the
||| given Query
|||
||| @name    The name of the attribute.
||| @convErr Convert XPathError to a local error type.
||| @qstr    The query to run.
||| @node    The thing we are querying.
getNamedAttrs : (name : String)
             -> (convErr : XPathError -> a)
             -> (qstr : String)
             -> (node : Document ty)
             -> {auto prfV : ValidNode ty}
             -> {auto prf : CanQuery ty}
             -> Either a (List String)
getNamedAttrs name convErr qstr doc = getNodeValues (Attr name) convErr qstr doc

||| Use XPath to get the *first* value for a named attribute in a node
||| satisfying the given Query
|||
||| @name    The name of the attribute.
||| @convErr Convert XPathError to a local error type.
||| @qstr    The query to run.
||| @node    The thing we are querying.
getNamedAttr : (name : String)
            -> (convErr : XPathError -> a)
            -> (qstr : String)
            -> (node : Document ty)
            -> {auto prfV : ValidNode ty}
            -> {auto prf : CanQuery ty}
            -> Either a String
getNamedAttr name convErr qstr doc = getNodeValue (Attr name) convErr qstr doc

||| Use XPath to get the text values for the nodes satisfying the
||| given Query
|||
||| @convErr Convert XPathError to a local error type.
||| @qstr    The query to run.
||| @node    The thing we are querying.
getTextNodes : (convErr : XPathError -> a)
            -> (qstr : String)
            -> (node : Document ty)
            -> {auto prfV : ValidNode ty}
            -> {auto prf : CanQuery ty}
            -> Either a (List String)
getTextNodes convErr qstr doc = getNodeValues Text convErr qstr doc

||| Use XPath to get the text value for the first node that satisfies
||| the given Query
|||
||| @convErr Convert XPathError to a local error type.
||| @qstr    The query to run.
||| @node    The thing we are querying.
getTextNode : (convErr : XPathError -> a)
           -> (qstr : String)
           -> (node : Document ty)
            -> {auto prfV : ValidNode ty}
           -> {auto prf : CanQuery ty}
           -> Either a String
getTextNode convErr qstr doc = getNodeValue Text convErr qstr doc

||| Use XPath to get the CData values for the nodes satisfying the
||| given Query
|||
||| @convErr Convert XPathError to a local error type.
||| @qstr    The query to run.
||| @node    The thing we are querying.
getCDataNodes : (convErr : XPathError -> a)
             -> (qstr : String)
             -> (node : Document ty)
             -> {auto prf : CanQuery ty}
            -> {auto prfV : ValidNode ty}
             -> Either a (List String)
getCDataNodes convErr qstr doc = getNodeValues CData convErr qstr doc

||| Use XPath to get the CData value for the first node that satisfies
||| the given Query
|||
||| @convErr Convert XPathError to a local error type.
||| @qstr    The query to run.
||| @node    The thing we are querying.
getCDataNode : (convErr : XPathError -> a)
            -> (qstr : String)
            -> (node : Document ty)
            -> {auto prfV : ValidNode ty}
            -> {auto prf : CanQuery ty}
            -> Either a String
getCDataNode convErr qstr doc = getNodeValue CData convErr qstr doc

-- --------------------------------------------------------------------- [ EOF ]
