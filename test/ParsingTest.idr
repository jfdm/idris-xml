module ParsingTest

import System

import Effects
import Effect.StdIO
import Effect.Exception
import Effect.System

import public Lightyear
import public Lightyear.Strings

import TestRunner

-- ----------------------------------------------------------- [ Parsing Tests ]

||| Expect a parsing test to pass.
parseEffTestGood : Show a => Parser a -> String -> EffTest
parseEffTestGood p s = case parse p s of
    Left err => raise $ "Expected passing test failed " ++ err
    Right re => putStrLn $ show re

||| Expect a test to fail
parseEffTestBad : Parser a -> String -> EffTest
parseEffTestBad p s = case parse p s of
    Left err => pure ()
    Right re => raise "Expected failing test passed..."


||| Expect a parsing test to pass.
parseTestGood : Show a => Parser a -> String -> IO ()
parseTestGood p s = case parse p s of
    Left err => putStrLn $ "------->  Expected passing test failed " ++ err
    Right re => putStrLn $ "Test Passed"

||| Expect a test to fail
parseTestBad : Parser a -> String -> IO ()
parseTestBad p s = case parse p s of
    Left err => putStrLn "Test Failed as Expected"
    Right re => putStrLn "-------> Expected failing test passed..."



-- --------------------------------------------------------------------- [ EPF ]
