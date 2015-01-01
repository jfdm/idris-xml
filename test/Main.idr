module Main

import System

import TestRunner
import ParsingTest

import XML.Types
import XML.Parser
import XML.Reader

-- --------------------------------------------------------------------- [ XML ]

test : Test
test = parseTestGood parseXML "<root><child>asas asas</child></root>"

-- -------------------------------------------------------------------- [ Main ]
main : IO ()
main = do
    run $ tests [test]
    exit 0

-- --------------------------------------------------------------------- [ EOF ]
