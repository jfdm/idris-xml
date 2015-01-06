module Main

import System

import TestRunner
import ParsingTest

import XML.Types
import XML.Parser
import XML.Reader

-- --------------------------------------------------------------------- [ XML ]

test : Test
test = do
  putStrLn "Test 0"
  parseTestGood node "<root><child>asas asas</child></root>"

test1 : Test
test1 = do
  putStrLn "Test 1"
  parseTestGood node "<!-- I am comment -->"

test2 : Test
test2 = do
  putStrLn "Test 2"
  parseTestBad node "<root><child></root></child>"

test3 : Test
test3 = do
  putStrLn "Test 3"
  parseTestGood node "<? target value=\"as\" ?>"

test4 : Test
test4 = do
  putStrLn "Test 4"
  parseTestGood node "<root><abc/><person name=\"bob\"><age>13</age></person></root>"

test5 : Test
test5 = do
  putStrLn "Test 5"
  parseTestGood xmlinfo "<?xml version=\"1.0\" encoding=\"utf-16\" standalone=\"true\" ?>"

test6 : Test
test6 = do
  putStrLn "Test 6"
  parseTestGood xmlinfo "<?xml version=\"1.0\" encoding=\"utf-16\"?>"

test7 : Test
test7 = do
  putStrLn "Test 7"
  parseTestGood doctype "<!DOCTYPE hello PUBLIC \"//PUBLIC/hello\" \"hello.dtd\">"

test8 : Test
test8 = do
  putStrLn "Test 8"
  parseTestGood node "<hello><world xmlns=\"worldnamespace\" first=\"1\" second=\"a\"><!--Just a - comment--><![CDATA[Some <CDATA&>]]><empty/><empty foo=\"bar\"/><characters>A</characters></world></hello>"

test9 : Test
test9 = do
  putStrLn "Text 9"
  parseTestGood parseXML "<?xml version=\"1.0\" encoding=\"utf-16\"?><!DOCTYPE hello PUBLIC \"//PUBLIC/hello\" \"hello.dtd\"><hello><world xmlns=\"worldnamespace\" first=\"1\" second=\"a\"><!--Just a - comment--><![CDATA[Some <CDATA&>]]><empty/><empty foo=\"bar\"/><characters>A</characters></world></hello>"

-- -------------------------------------------------------------------- [ Main ]
main : IO ()
main = do
    run $ tests [test, test1, test2, test3, test4, test5, test6, test7, test8, test9]
    exit 0

-- --------------------------------------------------------------------- [ EOF ]
