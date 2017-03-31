-- ---------------------------------------------------------- [ WellFormed.idr ]
-- Module    : WellFormed.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module XML.Test.WellFormed

import System

import Lightyear.Testing

import XML.DOM
import XML.Parser

export
runTests : IO ()
runTests = do
  putStrLn $ "=> Well Formed"

  Testing.runTests [
         parseTest "Well Formed 1" nodes "<root xmlns:h=\"http://www.w3.org/TR/html4/\" xmlns:f=\"http://www.w3schools.com/furniture\"/>"
       , parseTest "Well Formed 2" nodes "<library xmlns=\"http://eric.van-der-vlist.com/ns/library\" xmlns:hr=\"http://eric.van-der-vlist.com/ns/person\"/>"
       , parseTest "Well Formed 3" nodes "<root><child>asas asas</child></root>"
       , parseTest "Well Formed 4" nodes "<empty/>"
       , parseTest "Well Formed 5" nodes "<empty/><empty as=\"asas\" bs=\"de\"/>"


       , parseTest "Well Formed 6" nodes "<!-- I am comment -->"
       , parseTest "Well Formed 7" nodes "<hr:born>1950-10-04</hr:born>"

       , parseTestNot "Well Formed 8" nodes "<root><child></root></child>"

       , parseTest "Well Formed 9" nodes "<? target value=\"as\" ?>"
       , parseTest "Well Formed 10" nodes "<root><abc/><person name=\"bob\"><age>13</age></person></root>"

       , parseTest "Well Formed 11" xmlinfo "<?xml version=\"1.0\" encoding=\"utf-16\" standalone=\"true\" ?>"
       , parseTest "Well Formed 12" xmlinfo "<?xml version=\"1.0\" encoding=\"utf-16\"?>"
       , parseTest "Well Formed 13" doctype "<!DOCTYPE hello PUBLIC \"//PUBLIC/hello\" \"hello.dtd\">"
       , parseTest "Well Formed 14" nodes "<hello><world xmlns=\"worldnamespace\" first=\"1\" second=\"a\"><!--Just a - comment--><![CDATA[Some <CDATA&>]]><empty/><empty foo=\"bar\"/><characters>A</characters></world></hello>"

       , parseTest "Well Formed 15" parseXMLDoc "<?xml version=\"1.0\" encoding=\"utf-16\"?><!DOCTYPE hello PUBLIC \"//PUBLIC/hello\" \"hello.dtd\"><hello><world xmlns=\"worldnamespace\" first=\"1\" second=\"a\"><!--Just a - comment--><![CDATA[Some <CDATA&>]]><empty/><empty foo=\"bar\"/><characters>A</characters></world></hello>"

       , parseTest "Well Formed 16" nodes "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">asas</xs:schema>"
       ]
-- --------------------------------------------------------------------- [ EOF ]
