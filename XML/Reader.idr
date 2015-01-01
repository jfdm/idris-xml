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

import XML.Types
import XML.Parser

%access private

readFile : { [FILE_IO (OpenFile Read)] } Eff String
readFile = readAcc ""
  where
    readAcc : String -> { [FILE_IO (OpenFile Read)] } Eff String
    readAcc acc = if (not !eof)
                     then readAcc (acc ++ !readLine)
                     else pure acc

public
readXML : String -> { [FILE_IO ()] } Eff (Either String Document)
readXML f = do
    case !(open f Read) of
      True => do
        src <- readFile
        close
        case parse parseXML src of
          Left err  => pure $ Left err
          Right res => pure $ Right res
      False => pure $ Left "Error"
-- --------------------------------------------------------------------- [ EOF ]
