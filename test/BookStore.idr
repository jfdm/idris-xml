module BookStore

import XML.DOM
import XML.XPath


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


bstore : Document DOCUMENT
bstore = setRoot root $ mkDocument (mkQName "bookstore") Nothing
where
  root : Document ELEMENT
  root = mkSimpleElement "bookstore" <++> book1 <++> book2 <++> rogElem


people : Document DOCUMENT
people = setRoot root $ mkDocument (mkQName "people") Nothing
  where
    p1 : Document ELEMENT
    p1 = "person" <+=> "Michael"

    p2 : Document ELEMENT
    p2 = "person" <+=> "Eliezer"

    root : Document ELEMENT
    root = mkSimpleElement "people" <++> p1 <++> p2
