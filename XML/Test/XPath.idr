module XML.Test.XPath

import Text.PrettyPrint.WL

import Test.Unit.Display
import Test.Unit

import Commons.Text.Parser.Test

import XML.DOM
import XML.XPath

import XML.XPath.Parser


bstore : Document DOCUMENT
bstore = mkSimpleDocument root
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



people : Document DOCUMENT
people = mkSimpleDocument root
  where
    p1 : Document ELEMENT
    p1 = mkElementPrefix "person" "pre" <=> "Michael"

    p2 : Document ELEMENT
    p2 = "person" <+=> "Eliezer"

    root : Document ELEMENT
    root = mkSimpleElement "people" <++> p1 <++> p2


mkBook : String -> String -> String -> String -> String -> String -> Document ELEMENT
mkBook n a d f l p = mkSimpleElement "book"
    <++> ("title" <+=> n)
    <++> ("author" <+=> a)
    <++> ("desc" <+=> d)
    <++> ("format" <+=> l)
    <++> ("price" <+=> p)

||| http://en.wikibooks.org/wiki/XQuery/XPath_examples
books : Document DOCUMENT
books = mkSimpleDocument root
  where
    desc : Document ELEMENT
    desc = "desc" <+=> "A list of books useful for people first learning how to build web XML web applications."

    book1 : Document ELEMENT
    book1 = mkBook "XQuery"
                   "Priscilla Walmsley"
                   "This book is a highly detailed, through and complete tour of the W3C Query language.  It covers all the key aspects of the language as well as"
                   "Trade press"
                   "Commercial"
                   "49.95"

    book2 : Document ELEMENT
    book2 = mkBook "XQuery Examples"
                   "Chris Wallace"
                   "This book provides a variety of XQuery example programs and is designed to work with the eXist open-source native XML application server."
                   "Wiki-books"
                   "Creative Commons Sharealike 3.0 Attribution-Non-commercial"
                   "29.95"
    book3 : Document ELEMENT
    book3 = mkBook "XForms Tutorial and Cookbook"
                   "Dan McCreary"
                   "This book is an excellent guide for anyone that is just beginning to learn the XForms standard.  The book is focused on providing the reader with simple, but complete examples of how to create XForms web applications."
                   "Wikibook"
                   "Creative Commons Sharealike 3.0 Attribution-Non-commercial"
                   "29.95"

    book4 : Document ELEMENT
    book4 = mkBook "XRX: XForms, Rest and XQuery"
                   "Dan McCreary"
                   "This book is an overview of the key architectural and design patters."
                   "Wikibook"
                   "Creative Commons Sharealike 3.0 Attribution-Non-commercial"
                   "29.95"

    root : Document ELEMENT
    root = mkSimpleElement "books"
        <++> desc
        <++> book1
        <++> book2
        <++> book3
        <++> book4


myQuery : XPath ELEMENT QUERY
myQuery =  Query $
           (Root (Elem "bookstore"))
       </> ((Elem "book")
       </> (Elem "title"))

queryTest : String
         -> String
         -> Document DOCUMENT
         -> Bool
         -> IO Bool
queryTest title q doc chk = do
  putStrLn $ unwords ["Test:" , title]
  case queryDoc q doc of
    Left err => do
      when (chk) $ do
         let errMsg = vcat [
               text errLine
             , text "An error occured" <+> colon
             , indent 2 $ text (show err)
             , text errLine]
         putStrLn $ Default.toString errMsg
      if chk
        then pure False
        else pure True
    Right ( _ ** Empty) => do
      putStrLn "Empty"
      pure False
    Right (_ ** Nodes _ ns) => do
      printLn ns
      pure chk

export
runTests : IO ()
runTests = do
  putStrLn $ "=> XPath"

  NonReporting.runTests
      [ runParseTest showParseError $ parseTest "Query 1" (parseXPathStr) "/root/@test"
      , runParseTest showParseError $ parseTest "Query 2" (parseXPathStr) "/root/text()"
      , runParseTest showParseError $ parseTest "Query 3" (parseXPathStr) "/bookstore/book/title"
      , queryTest "Query 4" "/books/book/title" books True
      ]
