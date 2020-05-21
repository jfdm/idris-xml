-- ---------------------------------------------------------- [ WellFormed.idr ]
-- Module    : WellFormed.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module XML.Test.WellFormed

import System

import Text.PrettyPrint.WL

import Test.Unit.Display

import XML.Lexer
import XML.Parser
import XML.DOM
import XML.Serialise

import Test.Unit

genericTest' : (title : Maybe String)
           -> Bool
           -> (p : String -> Either XMLError (Document a))
           -> String
           -> IO Bool
genericTest' title chk p i = do
  putStrLn $ unwords ["Test:" , fromMaybe "Unnamed Test" title]
  case p i of
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
    Right _ => pure chk

parseTestNot : String
            -> (String -> Either XMLError (Document a))
            -> String
            -> IO Bool
parseTestNot title p i = genericTest' (Just title) False p i

parseTest : String
            -> (String -> Either XMLError (Document a))
            -> String
            -> IO Bool
parseTest title p i = genericTest' (Just title) True p i

parseTest' : String
          -> (Parser (Document a))
          -> String
          -> IO Bool
parseTest' title p i {a} =
   parseTest title
             foo
             i
  where
    foo : String -> Either XMLError (Document a)
    foo s = case parseString XMLLexer p s of
      Left err => Left (ParseError err)
      Right r => Right r

export
runTests : IO ()
runTests = do
  putStrLn $ "=> Well Formed"

  runTests [
         parseTest "Well Formed 1" (Snippet.fromString) "<root xmlns:h=\"http://www.w3.org/TR/html4/\" xmlns:f=\"http://www.w3schools.com/furniture\"/>\n"
       , parseTest "Well Formed 2" (Snippet.fromString) "<library xmlns=\"http://eric.van-der-vlist.com/ns/library\" xmlns:hr=\"http://eric.van-der-vlist.com/ns/person\"/>"
       , parseTest "Well Formed 3" (Snippet.fromString) "<root><child>asas asas</child></root>"
       , parseTest "Well Formed 4" (Snippet.fromString) "<empty/>"
       , parseTest "Well Formed 5" (Snippet.fromString) "<empty/><empty as=\"asas\" bs=\"de\"/>"


       , parseTest' "Well Formed 6" (comment) "<!-- I am comment -->"
       , parseTest "Well Formed 7" (Snippet.fromString) "<hr:born>1950-10-04</hr:born>"

       , parseTestNot "Well Formed 8" (Snippet.fromString) "<root><child></root></child>"

       , parseTest' "Well Formed 9" instruction "<? target value=\"as\" ?>"
       , parseTest "Well Formed 10" (Snippet.fromString) "<root><abc/><person name=\"bob\"><age>13</age></person></root>"

       , parseTest' "Well Formed 11" (xmlinfo) "<?xml version=\"1.0\" encoding=\"utf-16\" standalone=\"true\" ?>"
       , parseTest' "Well Formed 12" (xmlinfo) "<?xml version=\"1.0\" encoding=\"utf-16\"?>"
       , parseTest' "Well Formed 13" (doctype) "<!DOCTYPE hello PUBLIC \"//PUBLIC/hello\" \"hello.dtd\">"
       , parseTest "Well Formed 14" (Snippet.fromString) "<hello>\n<world xmlns=\"worldnamespace\" first=\"1\" second=\"a\">\n<!--Just a - comment-->\n<![CDATA[Some <CDATA&>]]>\n<empty/>\n<empty foo=\"bar\"/>\n<characters>A</characters>\n</world>\n</hello>"

       , parseTest "Well Formed 15" (Doc.fromString) "<?xml version=\"1.0\" encoding=\"utf-16\"?><!DOCTYPE hello PUBLIC \"//PUBLIC/hello\" \"hello.dtd\"><hello><world xmlns=\"worldnamespace\" first=\"1\" second=\"a\"><!--Just a - comment--><![CDATA[Some <CDATA&>]]><empty/><empty foo=\"bar\"/><characters>A</characters></world></hello>"

       , parseTest "Well Formed 16" (Snippet.fromString) "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">asas</xs:schema>"
       ]
-- --------------------------------------------------------------------- [ EOF ]
