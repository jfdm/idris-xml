-- ------------------------------------------------------- [ NotWellFormed.idr ]
-- Module    : NotWellFormed.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Tests taken from [here](http://www.jclark.com/xml/).
module XML.Test.NotWellFormed

import Test.Parsing

import XML.DOM
import XML.Parser

export
runTests : IO ()
runTests = do
  putStrLn $ heading "Not WellFormed"
  canParseNot Nothing (parseXMLDoc) "<?xml VERSION=\"1.0\"?> <doc></doc>"
  canParseNot Nothing (parseXMLDoc) "<?xml encoding=\"UTF-8\" version=\"1.0\"?> <doc></doc>"
  canParseNot Nothing (parseXMLDoc) "<?xml version=\"1.0\" encoding=\"UTF-8\" ?> <doc></doc>"
  canParseNot Nothing (parseXMLDoc) "<?xml version=\"1.0' encoding=\"UTF-8\" ?> <doc></doc>"
  canParseNot Nothing (parseXMLDoc) "<?xml version=\"1.0\" version=\"1.0\"?> <doc></doc>"
  canParseNot Nothing (parseXMLDoc) "<?xml version=\"1.0\" valid=\"no\" ?> <doc></doc>"
  canParseNot Nothing (parseXMLDoc) "<?xml version=\"1.0\" standalone=\"YES\" ?> <doc></doc>"
  canParseNot Nothing (parseXMLDoc) "<?xml version=\"1.0\" encoding=\" UTF-8\"?> <doc></doc>"
  canParseNot Nothing (parseXMLDoc) "<?xml version=\"1.0 \" ?> <doc></doc>"
  canParseNot Nothing (parseXMLDoc) "<?xml version=\"1.0\"?> <doc></doc>"
  canParseNot Nothing (parseXMLDoc) "<?xml encoding=\"UTF-8\"?> <doc></doc>"
  canParseNot Nothing (parseXMLDoc) "<?XML version=\"1.0\"?> <doc></doc>"
  canParseNot Nothing (parseXMLDoc) "<?xmL version=\"1.0\"?> <doc></doc>"


  canParseNot Nothing (nodes) "<doc> <doc ? <a</a> </doc>"
  canParseNot Nothing (nodes) "<doc> <.doc></.doc> </doc>"
  canParseNot Nothing (nodes) "<doc><? ?></doc>"
  canParseNot Nothing (nodes) "<doc><?target some data></doc>"
  canParseNot Nothing (nodes) "<doc><?target some data?</doc>"
  canParseNot Nothing (nodes) "<doc><!-- a comment -- another --></doc>"
  canParseNot Nothing (nodes) "<doc>&amp no refc</doc>"
  canParseNot Nothing (nodes) "<doc>&.entity;</doc>"
  canParseNot Nothing (nodes) "<doc>&#RE;</doc>"
  canParseNot Nothing (nodes) "<doc>A & B</doc>"

  canParseNot Nothing (nodes) "<doc a1></doc>"
  canParseNot Nothing (nodes) "<doc a1=v1></doc>"
  canParseNot Nothing (nodes) "<doc a1=\"v1'></doc>"
  canParseNot Nothing (nodes) "<doc a1=\"<foo>\"></doc>"
  canParseNot Nothing (nodes) "<doc a1=></doc>"
  canParseNot Nothing (nodes) "<doc a1=\"v1\" \"v2\"></doc>"
  canParseNot Nothing (nodes) "<doc><![CDATA[</doc>"
  canParseNot Nothing (nodes) "<doc><![CDATA [ stuff]]></doc>"
  canParseNot Nothing (nodes) "<doc></>"
  canParseNot Nothing (nodes) "<doc a1=\"A & B\"></doc>"

  canParseNot Nothing (nodes) "<doc a1=\"a&b\"></doc>"
  canParseNot Nothing (nodes) "<doc a1=\"&#123:\"></doc>"
  canParseNot Nothing (nodes) "<doc 12=\"34\"></doc>"
  canParseNot Nothing (nodes) "<doc> <123></123> </doc>"
  canParseNot Nothing (nodes) "<doc>]]></doc>"
  canParseNot Nothing (nodes) "<doc>]]]></doc>"
  canParseNot Nothing (nodes) "<doc> <!-- abc </doc>"
  canParseNot Nothing (nodes) "<doc> <?a pi that is not closed </doc>"
  canParseNot Nothing (nodes) "<doc>abc]]]>def</doc>"

  canParseNot Nothing (nodes) "<doc><?pi a form feed (asaas) is not allowed in a pi?></doc>"
  canParseNot Nothing (nodes) "<doc>1 < 2 but not in XML</doc>"
  canParseNot Nothing (nodes) "<doc></doc> Illegal data"
  canParseNot Nothing (nodes) "<doc></doc> &#32;"
  canParseNot Nothing (nodes) "<doc x=\"foo\" y=\"bar\" x=\"baz\"></doc>"

  canParseNot Nothing (nodes) "<doc><a></aa></doc>"
  canParseNot Nothing (nodes) "<doc></doc> <doc></doc>"
  canParseNot Nothing (nodes) "<doc/> <doc></doc>"
  canParseNot Nothing (nodes) "<doc/></doc/>"
  canParseNot Nothing (nodes) "<doc/> Illegal data"
  canParseNot Nothing (nodes) "<doc> <a/ </doc>"
  canParseNot Nothing (nodes) "<doc> <a/</a> </doc>"
  canParseNot Nothing (nodes) "<doc> <a / > </doc>"
  canParseNot Nothing (nodes) "<doc> </doc> <![CDATA[]]>"


  canParseNot Nothing (nodes) "<doc> <a><![CDATA[xyz]]]></a> <![CDATA[]]></a> </doc>"
  canParseNot Nothing (nodes) "<!-- a comment --> <![CDATA[]]> <doc></doc>"
  canParseNot Nothing (nodes) "<!-- a comment --> &#32; <doc></doc>"
  canParseNot Nothing (nodes) "<doc></DOC>"
  canParseNot Nothing (nodes) "<!-- a comment ending with three dashes ---> <doc></doc>"
  canParseNot Nothing (nodes) "<doc>&foo;</doc>"
  canParseNot Nothing (nodes) "<doc a=\"&foo;\"></doc>"
  canParseNot Nothing (nodes) "<doc>&#X58;</doc>"
  canParseNot Nothing (nodes) "<?pi stuff?> <![CDATA[]]> <doc> </doc>"
  canParseNot Nothing (nodes) "<?pi data?> &#32;<doc></doc>"
  canParseNot Nothing (nodes) "<doc> <![CDATA [ ]]> </doc>"

  canParseNot Nothing (nodes) "<doc> <![cdata[data]]> </doc>"
  canParseNot Nothing (nodes) "<!-- --> <?xml version=\"1.0\"?> <doc></doc>"
  canParseNot Nothing (nodes) "<doc> <?xml version=\"1.0\"?> </doc>"
  canParseNot Nothing (nodes) "<doc> </doc> <?xml version=\"1.0\"?>"
  canParseNot Nothing (nodes) "<doc> <?xMl version=\"1.0\"?> </doc>"
  canParseNot Nothing (nodes) "<doc> <?xmL?> </doc>"
  canParseNot Nothing (nodes) "<doc>/doc>"
  canParseNot Nothing (nodes) "<!--xxx --> <doc></doc>"
  canParseNot Nothing (nodes) "<?pas ?> <doc></doc>"
  canParseNot Nothing (nodes) "<doc a=\"a\"></doc>"
  canParseNot Nothing (nodes) "<doc><![CDATA[]]></doc>"
  canParseNot Nothing (nodes) "<!DOCTYPE a [ <!ELEMENT a EMPTY> <!ATTLIST a b CDATA #IMPLIED d CDATA #IMPLIED> ]> <a b=\"c\"d=\"e\"/>"

-- --------------------------------------------------------------------- [ EOF ]
