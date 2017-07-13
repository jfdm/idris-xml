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

public export
data XMLError : Type where
  ParseError     : String -> XMLError
  FileParseError : String -> String -> XMLError
  CannotReadFile : String -> FileError -> XMLError

public export
Show XMLError where
  show (ParseError err) = err
  show (FileParseError fn err) =
    unlines [ unwords ["Error parsing file", show fn, "error was"]
            , err
            ]
  show (CannotReadFile fn err) =
    unlines [ unwords ["Cannot read file:", show fn, "error was"]
            , show err
            ]


namespace Doc
  export
  fromString : String -> Either XMLError (Document DOCUMENT)
  fromString str =
    case parse parseXMLDoc str of
      Left err  => Left $ ParseError err
      Right res => pure $ res

export
readXMLDoc : String
           -> Eff (Either XMLError (Document DOCUMENT))
                  [FILE ()]
readXMLDoc f = parseFile CannotReadFile FileParseError parseXMLDoc f

namespace Snippet
  export
  fromString : String -> Either XMLError (Document ELEMENT)
  fromString str = do
    case parse parseXMLSnippet str of
      Left err  => Left $ ParseError err
      Right res => pure $ res

export
readXMLSnippet : String
              -> Eff (Either XMLError (Document ELEMENT))
                     [FILE ()]
readXMLSnippet f = parseFile CannotReadFile FileParseError parseXMLSnippet f

interface XMLReader a where
  fromSnippet : XMLElem -> Either XMLError a
  fromXMLDoc  : XMLDoc  -> Either XMLError a

interface XMLWriter a where
  toXML   : a -> XMLElem

  toXMLDoc : a -> XMLDoc
  toXMLDoc o = mkSimpleDocument (toXML o)




-- --------------------------------------------------------------------- [ EOF ]
