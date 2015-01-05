module BookStore

import XML.Types
import XML.DOM
import XML.XPath
-- % Make attributes work with nodes
bstore : Document
bstore = setRoot (appendChild book2 $ appendChild book1 $ createSimpleElement "bookstore")
                 (createEmptyDoc)
  where
    book1 : Node
    book1 =  NodeElement $ MkElement (createQName "book") Nil
               [appendChild (createTextNode "Harry Potter")
                            (createElementNode "title"),
                appendChild (createTextNode "29.99")
                            (createElementNode "price")
               ]
    book2 : Node
    book2 =  NodeElement $ MkElement (createQName "book") Nil
               [appendChild (createTextNode "Learning XML")
                            (createElementNode "title"),
                appendChild (createTextNode "39.35")
                            (createElementNode "price")
               ]
