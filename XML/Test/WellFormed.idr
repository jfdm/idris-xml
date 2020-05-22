-- ---------------------------------------------------------- [ WellFormed.idr ]
-- Module    : WellFormed.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module XML.Test.WellFormed

import Test.Unit

import Commons.Data.Location
import Commons.Text.Parser.Test

import XML.Lexer
import XML.Parser
import XML.DOM
import XML.Serialise

showParseError : Run.ParseError Token -> String
showParseError (FError e) = show e
showParseError (PError e) = unlines [maybe "" show (location e), error e]
showParseError (LError (MkLexFail l i)) = unlines [show l, show i]


export
runTests : IO ()
runTests = do
  putStrLn $ "=> Well Formed"

  NonReporting.runTests [
         runParseTest showParseError $ parseTest "Well Formed 1" (parseXMLSnippet) "<root xmlns:h=\"http://www.w3.org/TR/html4/\" xmlns:f=\"http://www.w3schools.com/furniture\"/>\n"
       , runParseTest showParseError $ parseTest "Well Formed 2" (parseXMLSnippet) "<library xmlns=\"http://eric.van-der-vlist.com/ns/library\" xmlns:hr=\"http://eric.van-der-vlist.com/ns/person\"/>"
       , runParseTest showParseError $ parseTest "Well Formed 3" (parseXMLSnippet) "<root><child>asas asas</child></root>"
       , runParseTest showParseError $ parseTest "Well Formed 4" (parseXMLSnippet) "<empty/>"
       , runParseTest showParseError $ parseTest "Well Formed 5" (parseXMLSnippet) "<empty/><empty as=\"asas\" bs=\"de\"/>"


       , runParseTest showParseError $ parseTest "Well Formed 6" (parseString XMLLexer comment) "<!-- I am comment -->"
       , runParseTest showParseError $ parseTest "Well Formed 7" (parseXMLSnippet) "<hr:born>1950-10-04</hr:born>"

       , runParseTest showParseError $ parseFail "Well Formed 8" (parseXMLSnippet) "<root><child></root></child>"

       , runParseTest showParseError $ parseTest "Well Formed 9" (parseString XMLLexer instruction) "<? target value=\"as\" ?>"
       , runParseTest showParseError $ parseTest"Well Formed 10" (parseXMLSnippet) "<root><abc/><person name=\"bob\"><age>13</age></person></root>"

       , runParseTest showParseError $ parseTest "Well Formed 11" (parseString XMLLexer xmlinfo) "<?xml version=\"1.0\" encoding=\"utf-16\" standalone=\"true\" ?>"
       , runParseTest showParseError $ parseTest "Well Formed 12" (parseString XMLLexer xmlinfo) "<?xml version=\"1.0\" encoding=\"utf-16\"?>"
       , runParseTest showParseError $ parseTest  "Well Formed 13" (parseString XMLLexer doctype) "<!DOCTYPE hello PUBLIC \"//PUBLIC/hello\" \"hello.dtd\">"
       , runParseTest showParseError $ parseTest "Well Formed 14" (parseXMLSnippet) "<hello>\n<world xmlns=\"worldnamespace\" first=\"1\" second=\"a\">\n<!--Just a - comment-->\n<![CDATA[Some <CDATA&>]]>\n<empty/>\n<empty foo=\"bar\"/>\n<characters>A</characters>\n</world>\n</hello>"

       , runParseTest showParseError $ parseTest "Well Formed 15" (parseXMLDoc) "<?xml version=\"1.0\" encoding=\"utf-16\"?><!DOCTYPE hello PUBLIC \"//PUBLIC/hello\" \"hello.dtd\"><hello><world xmlns=\"worldnamespace\" first=\"1\" second=\"a\"><!--Just a - comment--><![CDATA[Some <CDATA&>]]><empty/><empty foo=\"bar\"/><characters>A</characters></world></hello>"

       , runParseTest showParseError $ parseTest "Well Formed 16" (parseXMLSnippet) "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">asas</xs:schema>"
       ]
-- --------------------------------------------------------------------- [ EOF ]
