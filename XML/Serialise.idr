-- ----------------------------------------------------------- [ Serialise.idr ]
-- Module      : XML.Serialise
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- Typeclasses and code for serialising objects to xml and vice versa.
-- --------------------------------------------------------------------- [ EOH ]
module XML.Serialise

import Effects
import Effect.File
import Effect.Exception

import XML.Types
import XML.Parser
import XML.Reader

class Convertable a where
  toElement    : a -> Element
  fromElement  : Element -> Maybe a

readDocument : Convertable a => String -> {[FILE_IO (), EXCEPTION String]} Eff a
readDocument fname = do
   raw_doc <- readXMLFile fname
   case fromElement (root raw_doc) of
     Just e => pure e
     Nothing => raise "Invalid XML"


-- --------------------------------------------------------------------- [ EOF ]
