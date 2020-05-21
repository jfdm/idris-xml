-- -------------------------------------------------------------- [ Reader.idr ]
-- Module      : XML.reader
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module XML.Serialise

import Commons.Data.Location

import XML.DOM
import XML.Lexer
import XML.Parser

%access private

public export
data XMLError : Type where
  ParseError     : Run.ParseError Token -> XMLError
  FileParseError : Run.ParseError Token -> String -> XMLError
  CannotReadFile : String -> FileError -> XMLError

public export
Show XMLError where
  show (ParseError err) =
    unlines [ unwords ["Error parsing was"]
            , case err of
               (FError e) => show e
               (PError e) => unlines [maybe "" show (location e), error e]
               (LError (MkLexFail l i)) => unlines [show l, show i]
            ]
  show (FileParseError err fn) =
    unlines [ unwords ["Error parsing file", show fn, "error was"]
            , case err of
               (FError e) => show e
               (PError e) => unlines [maybe "" show (location e), error e]
               (LError (MkLexFail l i)) => unlines [show l, show i]
            ]
  show (CannotReadFile fn err) =
    unlines [ unwords ["Cannot read file:", show fn, "error was"]
            , show err
            ]


namespace Doc
  export
  fromString : String -> Either XMLError (Document DOCUMENT)
  fromString str =
    case parseXMLDoc str of
      Left err  => Left $ ParseError err
      Right res => pure $ res

namespace Snippet
  export
  fromString : String -> Either XMLError (Document ELEMENT)
  fromString str = do
    case parseXMLSnippet str of
      Left err  => Left $ ParseError err
      Right res => pure $ res


interface XMLReader a where
  fromSnippet : XMLElem -> Either XMLError a
  fromXMLDoc  : XMLDoc  -> Either XMLError a

interface XMLWriter a where
  toXML   : a -> XMLElem

  toXMLDoc : a -> XMLDoc
  toXMLDoc o = mkSimpleDocument (toXML o)




-- --------------------------------------------------------------------- [ EOF ]
