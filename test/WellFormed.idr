module WellFormed

import System

import TestRunner
import ParsingTest

import XML.DOM
import XML.Reader

tests1 : IO ()
tests1 = do
  putStrLn "Test 1"
  parseTestGood nodes "<root xmlns:h=\"http://www.w3.org/TR/html4/\" xmlns:f=\"http://www.w3schools.com/furniture\"/>"
  parseTestGood nodes "<library xmlns=\"http://eric.van-der-vlist.com/ns/library\" xmlns:hr=\"http://eric.van-der-vlist.com/ns/person\"/>"
  parseTestGood nodes "<root><child>asas asas</child></root>"
  parseTestGood nodes "<empty/>"
  parseTestGood nodes "<empty/><empty as=\"asas\" bs=\"de\"/>"

tests2 : IO ()
tests2 = do
  putStrLn "Test 2"
  parseTestGood nodes "<!-- I am comment -->"
  parseTestGood nodes "<hr:born>1950-10-04</hr:born>"
  parseTestBad nodes "<root><child></root></child>"
  parseTestGood nodes "<? target value=\"as\" ?>"
  parseTestGood nodes "<root><abc/><person name=\"bob\"><age>13</age></person></root>"

tests3 : IO ()
tests3 = do
  parseTestGood xmlinfo "<?xml version=\"1.0\" encoding=\"utf-16\" standalone=\"true\" ?>"
  parseTestGood xmlinfo "<?xml version=\"1.0\" encoding=\"utf-16\"?>"
  parseTestGood doctype "<!DOCTYPE hello PUBLIC \"//PUBLIC/hello\" \"hello.dtd\">"
  parseTestGood nodes "<hello><world xmlns=\"worldnamespace\" first=\"1\" second=\"a\"><!--Just a - comment--><![CDATA[Some <CDATA&>]]><empty/><empty foo=\"bar\"/><characters>A</characters></world></hello>"


tests4 : IO ()
tests4 = do
  parseTestGood parseXML "<?xml version=\"1.0\" encoding=\"utf-16\"?><!DOCTYPE hello PUBLIC \"//PUBLIC/hello\" \"hello.dtd\"><hello><world xmlns=\"worldnamespace\" first=\"1\" second=\"a\"><!--Just a - comment--><![CDATA[Some <CDATA&>]]><empty/><empty foo=\"bar\"/><characters>A</characters></world></hello>"
  putStrLn "Test 10"
  parseTestGood nodes "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">asas</xs:schema>"


WellFormedTests : IO ()
WellFormedTests = do
    tests1
    tests2
    tests3
    tests4

-- --------------------------------------------------------------------- [ EOF ]
