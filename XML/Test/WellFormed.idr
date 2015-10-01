-- ---------------------------------------------------------- [ WellFormed.idr ]
-- Module    : WellFormed.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module XML.Test.WellFormed

import System

import Test.Parsing

import XML.DOM
import XML.Parser

runTests : IO ()
runTests = do
  putStrLn $ heading "Well Formed"
  canParse Nothing nodes "<root xmlns:h=\"http://www.w3.org/TR/html4/\" xmlns:f=\"http://www.w3schools.com/furniture\"/>"
  canParse Nothing nodes "<library xmlns=\"http://eric.van-der-vlist.com/ns/library\" xmlns:hr=\"http://eric.van-der-vlist.com/ns/person\"/>"
  canParse Nothing nodes "<root><child>asas asas</child></root>"
  canParse Nothing nodes "<empty/>"
  canParse Nothing nodes "<empty/><empty as=\"asas\" bs=\"de\"/>"


  canParse Nothing nodes "<!-- I am comment -->"
  canParse Nothing nodes "<hr:born>1950-10-04</hr:born>"
  canParseNot Nothing nodes "<root><child></root></child>"
  canParse Nothing nodes "<? target value=\"as\" ?>"
  canParse Nothing nodes "<root><abc/><person name=\"bob\"><age>13</age></person></root>"

  canParse Nothing xmlinfo "<?xml version=\"1.0\" encoding=\"utf-16\" standalone=\"true\" ?>"
  canParse Nothing xmlinfo "<?xml version=\"1.0\" encoding=\"utf-16\"?>"
  canParse Nothing doctype "<!DOCTYPE hello PUBLIC \"//PUBLIC/hello\" \"hello.dtd\">"
  canParse Nothing nodes "<hello><world xmlns=\"worldnamespace\" first=\"1\" second=\"a\"><!--Just a - comment--><![CDATA[Some <CDATA&>]]><empty/><empty foo=\"bar\"/><characters>A</characters></world></hello>"

  canParse Nothing parseXMLDoc "<?xml version=\"1.0\" encoding=\"utf-16\"?><!DOCTYPE hello PUBLIC \"//PUBLIC/hello\" \"hello.dtd\"><hello><world xmlns=\"worldnamespace\" first=\"1\" second=\"a\"><!--Just a - comment--><![CDATA[Some <CDATA&>]]><empty/><empty foo=\"bar\"/><characters>A</characters></world></hello>"

  canParse Nothing nodes "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">asas</xs:schema>"

-- --------------------------------------------------------------------- [ EOF ]
