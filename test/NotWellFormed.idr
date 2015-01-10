||| Tests taken from [here](http://www.jclark.com/xml/).
module NotWellFormed

import System

import TestRunner
import ParsingTest

import XML.DOM
import XML.Reader

tests1 : IO ()
tests1 = do
  parseTestBad (parseXML) "<?xml VERSION=\"1.0\"?> <doc></doc>"
  parseTestBad (parseXML) "<?xml encoding=\"UTF-8\" version=\"1.0\"?> <doc></doc>"
  parseTestBad (parseXML) "<?xml version=\"1.0\"encoding=\"UTF-8\" ?> <doc></doc>"
  parseTestBad (parseXML) "<?xml version=\"1.0' encoding=\"UTF-8\" ?> <doc></doc>"
  parseTestBad (parseXML) "<?xml version=\"1.0\" version=\"1.0\"?> <doc></doc>"
  parseTestBad (parseXML) "<?xml version=\"1.0\" valid=\"no\" ?> <doc></doc>"
  parseTestBad (parseXML) "<?xml version=\"1.0\" standalone=\"YES\" ?> <doc></doc>"
  parseTestBad (parseXML) "<?xml version=\"1.0\" encoding=\" UTF-8\"?> <doc></doc>"
  parseTestBad (parseXML) "<?xml version=\"1.0 \" ?> <doc></doc>"
  parseTestBad (parseXML) "<?xml version=\"1.0\"?> <doc></doc>"
  parseTestBad (parseXML) "<?xml encoding=\"UTF-8\"?> <doc></doc>"
  parseTestBad (parseXML) "<?XML version=\"1.0\"?> <doc></doc>"
  parseTestBad (parseXML) "<?xmL version=\"1.0\"?> <doc></doc>"


tests2 : IO ()
tests2 = do
  parseTestBad (nodes) "<doc> <doc ? <a</a> </doc>"
  parseTestBad (nodes) "<doc> <.doc></.doc> </doc>"
  parseTestBad (nodes) "<doc><? ?></doc>"
  parseTestBad (nodes) "<doc><?target some data></doc>"
  parseTestBad (nodes) "<doc><?target some data?</doc>"
  parseTestBad (nodes) "<doc><!-- a comment -- another --></doc>"
  parseTestBad (nodes) "<doc>&amp no refc</doc>"
  parseTestBad (nodes) "<doc>&.entity;</doc>"
  parseTestBad (nodes) "<doc>&#RE;</doc>"
  parseTestBad (nodes) "<doc>A & B</doc>"

tests3 : IO ()
tests3 = do
  parseTestBad (nodes) "<doc a1></doc>"
  parseTestBad (nodes) "<doc a1=v1></doc>"
  parseTestBad (nodes) "<doc a1=\"v1'></doc>"
  parseTestBad (nodes) "<doc a1=\"<foo>\"></doc>"
  parseTestBad (nodes) "<doc a1=></doc>"
  parseTestBad (nodes) "<doc a1=\"v1\" \"v2\"></doc>"
  parseTestBad (nodes) "<doc><![CDATA[</doc>"
  parseTestBad (nodes) "<doc><![CDATA [ stuff]]></doc>"
  parseTestBad (nodes) "<doc></>"
  parseTestBad (nodes) "<doc a1=\"A & B\"></doc>"

tests4 : IO ()
tests4 = do
  parseTestBad (nodes) "<doc a1=\"a&b\"></doc>"
  parseTestBad (nodes) "<doc a1=\"&#123:\"></doc>"
  parseTestBad (nodes) "<doc 12=\"34\"></doc>"
  parseTestBad (nodes) "<doc> <123></123> </doc>"
  parseTestBad (nodes) "<doc>]]></doc>"
  parseTestBad (nodes) "<doc>]]]></doc>"
  parseTestBad (nodes) "<doc> <!-- abc </doc>"
  parseTestBad (nodes) "<doc> <?a pi that is not closed </doc>"
  parseTestBad (nodes) "<doc>abc]]]>def</doc>"

tests5 : IO ()
tests5 = do
  parseTestBad (nodes) "<doc>A form feed (asaas) is not legal in data</doc>"
  parseTestBad (nodes) "<doc><?pi a form feed (asaas) is not allowed in a pi?></doc>"
  parseTestBad (nodes) "<doc><!-- a form feed (asaas) is not allowed in a comment --></doc>"
  parseTestBad (nodes) "<doc>abasaddef</doc>"
  parseTestBad (nodes) "<docasaas>A form-feed is not white space or a name character</docasaas>"
  parseTestBad (nodes) "<doc>1 < 2 but not in XML</doc>"
  parseTestBad (nodes) "<doc></doc> Illegal data"
  parseTestBad (nodes) "<doc></doc> &#32;"
  parseTestBad (nodes) "<doc x=\"foo\" y=\"bar\" x=\"baz\"></doc>"

tests6 : IO ()
tests6 = do
  parseTestBad (nodes) "<doc><a></aa></doc>"
  parseTestBad (nodes) "<doc></doc> <doc></doc>"
  parseTestBad (nodes) "<doc/> <doc></doc>"
  parseTestBad (nodes) "<doc/></doc/>"
  parseTestBad (nodes) "<doc/> Illegal data"
  parseTestBad (nodes) "<doc/><doc/>"
  parseTestBad (nodes) "<doc> <a/ </doc>"
  parseTestBad (nodes) "<doc> <a/</a> </doc>"
  parseTestBad (nodes) "<doc> <a / > </doc>"
  parseTestBad (nodes) "<doc> </doc> <![CDATA[]]>"

tests7 : IO ()
tests7 = do
  parseTestBad (nodes) "<doc> <a><![CDATA[xyz]]]></a> <![CDATA[]]></a> </doc>"
  parseTestBad (nodes) "<!-- a comment --> <![CDATA[]]> <doc></doc>"
  parseTestBad (nodes) "<!-- a comment --> &#32; <doc></doc>"
  parseTestBad (nodes) "<doc></DOC>"
  parseTestBad (nodes) "<!-- a comment ending with three dashes ---> <doc></doc>"
  parseTestBad (nodes) "<doc>&foo;</doc>"
  parseTestBad (nodes) "<doc a=\"&foo;\"></doc>"
  parseTestBad (nodes) "<doc>&#X58;</doc>"
  parseTestBad (nodes) "<?pi stuff?> <![CDATA[]]> <doc> </doc>"
  parseTestBad (nodes) "<?pi data?> &#32;<doc></doc>"
  parseTestBad (nodes) "<doc> <![CDATA [ ]]> </doc>"

tests8 : IO ()
tests8 = do
  parseTestBad (nodes) "<doc> <![cdata[data]]> </doc>"
  parseTestBad (nodes) "<!-- --> <?xml version=\"1.0\"?> <doc></doc>"
  parseTestBad (nodes) "<doc> <?xml version=\"1.0\"?> </doc>"
  parseTestBad (nodes) "<doc> </doc> <?xml version=\"1.0\"?>"
  parseTestBad (nodes) "<doc> <?xMl version=\"1.0\"?> </doc>"
  parseTestBad (nodes) "<doc> <?xmL?> </doc>"
  parseTestBad (nodes) "<doc></doc>"
  parseTestBad (nodes) "<!--xxx --> <doc></doc>"
  parseTestBad (nodes) "<?pas ?> <doc></doc>"
  parseTestBad (nodes) "<doc a=\"a\"></doc>"
  parseTestBad (nodes) "<doc><![CDATA[]]></doc>"
  parseTestBad (nodes) "<!DOCTYPE a [ <!ELEMENT a EMPTY> <!ATTLIST a b CDATA #IMPLIED d CDATA #IMPLIED> ]> <a b=\"c\"d=\"e\"/>"


NotWellFormedTests : IO ()
NotWellFormedTests = do
  tests1
  tests2
  tests3
  tests4
  tests5
  tests6
  tests7
  tests8

-- --------------------------------------------------------------------- [ EOF ]
