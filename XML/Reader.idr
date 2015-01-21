-- -------------------------------------------------------------- [ Reader.idr ]
-- Module      : XML.reader
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module XML.Reader

import Lightyear
import Lightyear.Strings

import Effects
import Effect.File
import Effect.Exception

import XML.DOM
import public XML.Parser

%access private

readFile : { [FILE_IO (OpenFile Read)] } Eff String
readFile = readAcc ""
  where
    readAcc : String -> { [FILE_IO (OpenFile Read)] } Eff String
    readAcc acc = if (not !eof)
                     then readAcc (acc ++ !readLine)
                     else pure acc

public
readDocString : String -> {[EXCEPTION String]} Eff (Document DOCUMENT)
readDocString str = do
  case parse parseXML str of
    Left err  => raise err
    Right res => pure $ res

public
readDocFile : String -> { [EXCEPTION String, FILE_IO ()] } Eff (Document DOCUMENT)
readDocFile f = do
    case !(open f Read) of
      True => do
        src <- readFile
        close
        readDocString src
      False => raise "Unable to read XML file"

-- --------------------------------------------------------------------- [ EOF ]
