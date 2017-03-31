-- ------------------------------------------------------- [ NotWellFormed.idr ]
-- Module    : NotWellFormed.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Tests taken from [here](http://www.jclark.com/xml/).
module XML.Test.NotWellFormed

import Lightyear.Testing

import XML.DOM
import XML.Parser

export
runTests : IO ()
runTests = do
  putStrLn "=> Not WellFormed"

  runTests [
       parseTestNot "Not Well Formed 1" (parseXMLDoc) "<?xml VERSION=\"1.0\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 2" (parseXMLDoc) "<?xml encoding=\"UTF-8\" version=\"1.0\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 3" (parseXMLDoc) "<?xml version=\"1.0\" encoding=\"UTF-8\" ?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 4" (parseXMLDoc) "<?xml version=\"1.0' encoding=\"UTF-8\" ?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 5" (parseXMLDoc) "<?xml version=\"1.0\" version=\"1.0\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 6" (parseXMLDoc) "<?xml version=\"1.0\" valid=\"no\" ?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 7" (parseXMLDoc) "<?xml version=\"1.0\" standalone=\"YES\" ?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 8" (parseXMLDoc) "<?xml version=\"1.0\" encoding=\" UTF-8\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 9" (parseXMLDoc) "<?xml version=\"1.0 \" ?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 10" (parseXMLDoc) "<?xml version=\"1.0\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 11" (parseXMLDoc) "<?xml encoding=\"UTF-8\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 12" (parseXMLDoc) "<?XML version=\"1.0\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 13" (parseXMLDoc) "<?xmL version=\"1.0\"?> <doc></doc>"

    ,  parseTestNot "Not Well Formed 14" (nodes) "<doc> <doc ? <a</a> </doc>"
    ,  parseTestNot "Not Well Formed 15" (nodes) "<doc> <.doc></.doc> </doc>"
    ,  parseTestNot "Not Well Formed 16" (nodes) "<doc><? ?></doc>"
    ,  parseTestNot "Not Well Formed 17" (nodes) "<doc><?target some data></doc>"
    ,  parseTestNot "Not Well Formed 18" (nodes) "<doc><?target some data?</doc>"
    ,  parseTestNot "Not Well Formed 19" (nodes) "<doc><!-- a comment -- another --></doc>"
    ,  parseTestNot "Not Well Formed 20" (nodes) "<doc>&amp no refc</doc>"
    ,  parseTestNot "Not Well Formed 21" (nodes) "<doc>&.entity;</doc>"
    ,  parseTestNot "Not Well Formed 22" (nodes) "<doc>&#RE;</doc>"
    ,  parseTestNot "Not Well Formed 23" (nodes) "<doc>A & B</doc>"

    ,  parseTestNot "Not Well Formed 24" (nodes) "<doc a1></doc>"
    ,  parseTestNot "Not Well Formed 25" (nodes) "<doc a1=v1></doc>"
    ,  parseTestNot "Not Well Formed 26" (nodes) "<doc a1=\"v1'></doc>"
    ,  parseTestNot "Not Well Formed 27" (nodes) "<doc a1=\"<foo>\"></doc>"
    ,  parseTestNot "Not Well Formed 28" (nodes) "<doc a1=></doc>"
    ,  parseTestNot "Not Well Formed 29" (nodes) "<doc a1=\"v1\" \"v2\"></doc>"
    ,  parseTestNot "Not Well Formed 30" (nodes) "<doc><![CDATA[</doc>"
    ,  parseTestNot "Not Well Formed 31" (nodes) "<doc><![CDATA [ stuff]]></doc>"
    ,  parseTestNot "Not Well Formed 32" (nodes) "<doc></>"
    ,  parseTestNot "Not Well Formed 33" (nodes) "<doc a1=\"A & B\"></doc>"

    ,  parseTestNot "Not Well Formed 34" (nodes) "<doc a1=\"a&b\"></doc>"
    ,  parseTestNot "Not Well Formed 35" (nodes) "<doc a1=\"&#123:\"></doc>"
    ,  parseTestNot "Not Well Formed 36" (nodes) "<doc 12=\"34\"></doc>"
    ,  parseTestNot "Not Well Formed 37" (nodes) "<doc> <123></123> </doc>"
    ,  parseTestNot "Not Well Formed 38" (nodes) "<doc>]]></doc>"
    ,  parseTestNot "Not Well Formed 39" (nodes) "<doc>]]]></doc>"
    ,  parseTestNot "Not Well Formed 40" (nodes) "<doc> <!-- abc </doc>"
    ,  parseTestNot "Not Well Formed 41" (nodes) "<doc> <?a pi that is not closed </doc>"
    ,  parseTestNot "Not Well Formed 42" (nodes) "<doc>abc]]]>def</doc>"

    ,  parseTestNot "Not Well Formed 43" (nodes) "<doc><?pi a form feed (asaas) is not allowed in a pi?></doc>"
    ,  parseTestNot "Not Well Formed 44" (nodes) "<doc>1 < 2 but not in XML</doc>"
    ,  parseTestNot "Not Well Formed 45" (nodes) "<doc></doc> Illegal data"
    ,  parseTestNot "Not Well Formed 46" (nodes) "<doc></doc> &#32;"
    ,  parseTestNot "Not Well Formed 47" (nodes) "<doc x=\"foo\" y=\"bar\" x=\"baz\"></doc>"

    ,  parseTestNot "Not Well Formed 48" (nodes) "<doc><a></aa></doc>"
    ,  parseTestNot "Not Well Formed 49" (nodes) "<doc></doc> <doc></doc>"
    ,  parseTestNot "Not Well Formed 50" (nodes) "<doc/> <doc></doc>"
    ,  parseTestNot "Not Well Formed 51" (nodes) "<doc/></doc/>"
    ,  parseTestNot "Not Well Formed 52" (nodes) "<doc/> Illegal data"
    ,  parseTestNot "Not Well Formed 53" (nodes) "<doc> <a/ </doc>"
    ,  parseTestNot "Not Well Formed 54" (nodes) "<doc> <a/</a> </doc>"
    ,  parseTestNot "Not Well Formed 55" (nodes) "<doc> <a / > </doc>"
    ,  parseTestNot "Not Well Formed 56" (nodes) "<doc> </doc> <![CDATA[]]>"


    ,  parseTestNot "Not Well Formed 57" (nodes) "<doc> <a><![CDATA[xyz]]]></a> <![CDATA[]]></a> </doc>"
    ,  parseTestNot "Not Well Formed 58" (nodes) "<!-- a comment --> <![CDATA[]]> <doc></doc>"
    ,  parseTestNot "Not Well Formed 59" (nodes) "<!-- a comment --> &#32; <doc></doc>"
    ,  parseTestNot "Not Well Formed 60" (nodes) "<doc></DOC>"
    ,  parseTestNot "Not Well Formed 61" (nodes) "<!-- a comment ending with three dashes ---> <doc></doc>"
    ,  parseTestNot "Not Well Formed 62" (nodes) "<doc>&foo;</doc>"
    ,  parseTestNot "Not Well Formed 63" (nodes) "<doc a=\"&foo;\"></doc>"
    ,  parseTestNot "Not Well Formed 64" (nodes) "<doc>&#X58;</doc>"
    ,  parseTestNot "Not Well Formed 65" (nodes) "<?pi stuff?> <![CDATA[]]> <doc> </doc>"
    ,  parseTestNot "Not Well Formed 66" (nodes) "<?pi data?> &#32;<doc></doc>"
    ,  parseTestNot "Not Well Formed 67" (nodes) "<doc> <![CDATA [ ]]> </doc>"

    ,  parseTestNot "Not Well Formed 68" (nodes) "<doc> <![cdata[data]]> </doc>"
    ,  parseTestNot "Not Well Formed 69" (nodes) "<!-- --> <?xml version=\"1.0\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 70" (nodes) "<doc> <?xml version=\"1.0\"?> </doc>"
    ,  parseTestNot "Not Well Formed 71" (nodes) "<doc> </doc> <?xml version=\"1.0\"?>"
    ,  parseTestNot "Not Well Formed 72" (nodes) "<doc> <?xMl version=\"1.0\"?> </doc>"
    ,  parseTestNot "Not Well Formed 73" (nodes) "<doc> <?xmL?> </doc>"
    ,  parseTestNot "Not Well Formed 74" (nodes) "<doc>/doc>"
    ,  parseTestNot "Not Well Formed 75" (nodes) "<!--xxx --> <doc></doc>"
    ,  parseTestNot "Not Well Formed 76" (nodes) "<?pas ?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 77" (nodes) "<doc a=\"a\"></doc>"
    ,  parseTestNot "Not Well Formed 78" (nodes) "<doc><![CDATA[]]></doc>"
    ,  parseTestNot "Not Well Formed 79" (nodes) "<!DOCTYPE a [ <!ELEMENT a EMPTY> <!ATTLIST a b CDATA #IMPLIED d CDATA #IMPLIED> ]> <a b=\"c\"d=\"e\"/>"
    ]
-- --------------------------------------------------------------------- [ EOF ]
