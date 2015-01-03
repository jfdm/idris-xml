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
  parseTestGood node "<? target data ?>"

test4 : Test
test4 = do
  putStrLn "Test 4"
  parseTestBad node "<root><abc/><person name=\"bob\"><age = \"13\"/></person></root>"


-- -------------------------------------------------------------------- [ Main ]
main : IO ()
main = do
    run $ tests [test, test1, test2, test3, test4]
    exit 0

-- --------------------------------------------------------------------- [ EOF ]
