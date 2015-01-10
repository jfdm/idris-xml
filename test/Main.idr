module Main

import System

import TestRunner
import ParsingTest

import XML.DOM
import XML.Reader

import WellFormed
import NotWellFormed

-- -------------------------------------------------------------------- [ Main ]
main : IO ()
main = do
    WellFormedTests
    NotWellFormedTests
    exit 0

-- --------------------------------------------------------------------- [ EOF ]
