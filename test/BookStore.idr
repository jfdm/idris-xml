module BookStore

import XML.Types
import XML.DOM
import XML.XPath

-- % Make attributes work with nodes
{-
bstore : Document
bstore = setRoot (createSimpleElement "bookstore" <++> rogElem <++> book2 <++> book1)
                 (createEmptyDoc)
  where
    rogElem : Node
    rogElem = "isbn" <+=> "123"

    book1 : Node
    book1 =  NodeElement $ MkElement (createQName "book") Nil
               [ "title" <+=>  "Harry Potter",
                 "price" <+=> "29.99"
               ]

    book2 : Node
    book2 =  NodeElement $ MkElement (createQName "book") Nil
               [ "title" <+=>  "Learning XML",
                 "price" <+=> "39.35",
                 "isbn"  <+=> "123456"
               ]

temp : XPath QUERY
temp = Query $ Elem "bookstore" <//> Elem "book"
-}
bstore : Document DOCUMENT
bstore = MkDocument (MkXMLInfo "" "" True) Nothing Nil Nothing root
  where
    b1 : Document ELEMENT
    b1 = Element (MkQName "book" Nothing Nothing) Nil
           [mkNode $ Element (MkQName "title" Nothing Nothing) Nil [mkNode $ Text "Harry Potter"],
            mkNode $ Element (MkQName "price" Nothing Nothing) Nil [mkNode $ Text "29.99",
                                                                    mkNode $ Comment "as"]
           ]

    bs : NodeList
    bs = [mkNode b1, mkNode b1]

    root : Document ELEMENT
    root = Element (MkQName "bookstore" Nothing Nothing) Nil bs
