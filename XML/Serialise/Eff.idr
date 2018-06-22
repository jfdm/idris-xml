-- ----------------------------------------------------------------- [ Eff.idr ]
-- Module      : XML.Reader.Eff
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module XML.Serialise.Eff

import Lightyear
import Lightyear.Strings
import Lightyear.StringFile

import Effects
import Effect.File
import Effect.Exception

import XML.DOM
import XML.Parser
import XML.Serialise

%access private

export
readXMLDoc : String
           -> Eff (Either XMLError (Document DOCUMENT))
                  [FILE ()]
readXMLDoc f = parseFile CannotReadFile FileParseError parseXMLDoc f

export
readXMLSnippet : String
              -> Eff (Either XMLError (Document ELEMENT))
                     [FILE ()]
readXMLSnippet f = parseFile CannotReadFile FileParseError parseXMLSnippet f

-- --------------------------------------------------------------------- [ EOF ]
