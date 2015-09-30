module QueryDoc

import System

import Effects
import Effect.System
import Effect.File
import Effect.StdIO
import Effect.Exception

import XML.DOM
import XML.Reader
import XML.XPath

%default partial

XEffs : List EFFECT
XEffs = [SYSTEM, FILE_IO (), STDIO, EXCEPTION String]

printRes : List XMLNode -> Eff () XEffs
printRes Nil     = pure ()
printRes (x::xs) = do
  putStrLn $ show @{xml} x
  printRes xs

eMain : Eff () XEffs
eMain = do
    [prog, fn, qstr] <- getArgs | Nil   => raise "Cannot happen!"
                                | [x]   => raise "Need doc and query"
                                | [x,y] => raise "Need Query"
                                | _     => raise "To manny arguments"

    case !(readXMLDoc fn) of
      Left err  => raise $ show err
      Right doc => do
        case query qstr doc of
          Left err  => raise $ show err
          Right Nil => raise "Nothing Found"
          Right xs  => printRes xs

namespace Main
  main : IO ()
  main = do
    run eMain
    exit 0
-- --------------------------------------------------------------------- [ EOF ]
