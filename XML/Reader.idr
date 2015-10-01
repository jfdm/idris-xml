-- -------------------------------------------------------------- [ Reader.idr ]
-- Module      : XML.reader
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module XML.Reader

import Lightyear
import Lightyear.Strings
import Lightyear.StringFile

import Effects
import Effect.File
import Effect.Exception

import XML.DOM
import XML.Parser

%access private

public
data XMLError : Type where
  ParseError     : String -> XMLError
  FileParseError : String -> String -> XMLError
  CannotReadFile : String -> XMLError

instance Show XMLError where
  show (ParseError err) = err
  show (FileParseError fn err) =
    unlines [ unwords ["Error parsing file", show fn, "error was"]
            , err]
  show (CannotReadFile fn) = unwords ["Cannot read file:", show fn]


namespace Doc
  public
  fromString : String -> Either XMLError (Document DOCUMENT)
  fromString str =
    case parse parseXMLDoc str of
      Left err  => Left $ ParseError err
      Right res => pure $ res

public
readXMLDoc : String
           -> Eff (Either XMLError (Document DOCUMENT))
                  [FILE_IO ()]
readXMLDoc f = parseFile CannotReadFile FileParseError parseXMLDoc f

namespace Snippet
  public
  fromString : String -> Either XMLError (Document ELEMENT)
  fromString str = do
    case parse parseXMLSnippet str of
      Left err  => Left $ ParseError err
      Right res => pure $ res

public
readXMLSnippet : String
              -> Eff (Either XMLError (Document ELEMENT))
                     [FILE_IO ()]
readXMLSnippet f = parseFile CannotReadFile FileParseError parseXMLSnippet f

class XMLReader a where
  fromSnippet : XMLElem -> Either XMLError a
  fromXMLDoc  : XMLDoc  -> Either XMLError a

class XMLWriter a where
  toXML   : a -> XMLElem

  toXMLDoc : a -> XMLDoc
  toXMLDoc o = mkDocument (toXML o)




-- --------------------------------------------------------------------- [ EOF ]
