module Main

import System

import TestRunner
import ParsingTest

import XML.DOM
import XML.Reader

-- --------------------------------------------------------------------- [ XML ]

test : Test
test = do
  putStrLn "Test 0"
  parseTestGood nodes "<root><child>asas asas</child></root>"

test1 : Test
test1 = do
  putStrLn "Test 1"
  parseTestGood nodes "<!-- I am comment -->"

test2 : Test
test2 = do
  putStrLn "Test 2"
  parseTestBad nodes "<root><child></root></child>"

test3 : Test
test3 = do
  putStrLn "Test 3"
  parseTestGood nodes "<? target value=\"as\" ?>"

test4 : Test
test4 = do
  putStrLn "Test 4"
  parseTestGood nodes "<root><abc/><person name=\"bob\"><age>13</age></person></root>"

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
  parseTestGood nodes "<hello><world xmlns=\"worldnamespace\" first=\"1\" second=\"a\"><!--Just a - comment--><![CDATA[Some <CDATA&>]]><empty/><empty foo=\"bar\"/><characters>A</characters></world></hello>"

test9 : Test
test9 = do
  putStrLn "Text 9"
  parseTestGood parseXML "<?xml version=\"1.0\" encoding=\"utf-16\"?><!DOCTYPE hello PUBLIC \"//PUBLIC/hello\" \"hello.dtd\"><hello><world xmlns=\"worldnamespace\" first=\"1\" second=\"a\"><!--Just a - comment--><![CDATA[Some <CDATA&>]]><empty/><empty foo=\"bar\"/><characters>A</characters></world></hello>"

-- -------------------------------------------------------------------- [ Main ]
main : IO ()
main = do
    run $ tests [test]
    run $ tests [test1]
    run $ tests [test2]
    run $ tests [test3]
    run $ tests [test4]
    run $ tests [test5]
    run $ tests [test6]
    run $ tests [test7]
    run $ tests [test8]
    run $ tests [test9]
    exit 0

-- --------------------------------------------------------------------- [ EOF ]
