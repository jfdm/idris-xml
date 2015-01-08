module BookStore

import XML.Types
import XML.DOM
import XML.XPath

bstore : Document DOCUMENT
bstore = setRoot root $ mkDocument (mkQName "bookstore") Nothing
  where
    rogElem : Document ELEMENT
    rogElem = "isbn" <+=> "123"

    book1 : Document ELEMENT
    book1 = mkSimpleElement "book"
          <++> ("title" <+=>  "Harry Potter")
          <++> ("price" <+=> "29.99")


    book2 : Document ELEMENT
    book2 = mkSimpleElement "book"
          <++> ("title" <+=>  "Learning XML")
          <++> ("price" <+=> "39.35")
          <++> ("isbn"  <+=> "123456")

    root : Document ELEMENT
    root = mkSimpleElement "bookstore" <++> book1 <++> book2 <++> rogElem
