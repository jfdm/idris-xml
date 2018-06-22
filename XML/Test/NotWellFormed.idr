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

    ,  parseTestNot "Not Well Formed 14" (parseXMLSnippet) "<doc> <doc ? <a</a> </doc>"
    ,  parseTestNot "Not Well Formed 15" (parseXMLSnippet) "<doc> <.doc></.doc> </doc>"
    ,  parseTestNot "Not Well Formed 16" (parseXMLSnippet) "<doc><? ?></doc>"
    ,  parseTestNot "Not Well Formed 17" (parseXMLSnippet) "<doc><?target some data></doc>"
    ,  parseTestNot "Not Well Formed 18" (parseXMLSnippet) "<doc><?target some data?</doc>"
    ,  parseTestNot "Not Well Formed 19" (parseXMLSnippet) "<doc><!-- a comment -- another --></doc>"
    ,  parseTestNot "Not Well Formed 20" (parseXMLSnippet) "<doc>&amp no refc</doc>"
    ,  parseTestNot "Not Well Formed 21" (parseXMLSnippet) "<doc>&.entity;</doc>"
    ,  parseTestNot "Not Well Formed 22" (parseXMLSnippet) "<doc>&#RE;</doc>"
    ,  parseTestNot "Not Well Formed 23" (parseXMLSnippet) "<doc>A & B</doc>"

    ,  parseTestNot "Not Well Formed 24" (parseXMLSnippet) "<doc a1></doc>"
    ,  parseTestNot "Not Well Formed 25" (parseXMLSnippet) "<doc a1=v1></doc>"
    ,  parseTestNot "Not Well Formed 26" (parseXMLSnippet) "<doc a1=\"v1'></doc>"
    ,  parseTestNot "Not Well Formed 27" (parseXMLSnippet) "<doc a1=\"<foo>\"></doc>"
    ,  parseTestNot "Not Well Formed 28" (parseXMLSnippet) "<doc a1=></doc>"
    ,  parseTestNot "Not Well Formed 29" (parseXMLSnippet) "<doc a1=\"v1\" \"v2\"></doc>"
    ,  parseTestNot "Not Well Formed 30" (parseXMLSnippet) "<doc><![CDATA[</doc>"
    ,  parseTestNot "Not Well Formed 31" (parseXMLSnippet) "<doc><![CDATA [ stuff]]></doc>"
    ,  parseTestNot "Not Well Formed 32" (parseXMLSnippet) "<doc></>"
    ,  parseTestNot "Not Well Formed 33" (parseXMLSnippet) "<doc a1=\"A & B\"></doc>"

    ,  parseTestNot "Not Well Formed 34" (parseXMLSnippet) "<doc a1=\"a&b\"></doc>"
    ,  parseTestNot "Not Well Formed 35" (parseXMLSnippet) "<doc a1=\"&#123:\"></doc>"
    ,  parseTestNot "Not Well Formed 36" (parseXMLSnippet) "<doc 12=\"34\"></doc>"
    ,  parseTestNot "Not Well Formed 37" (parseXMLSnippet) "<doc> <123></123> </doc>"
    ,  parseTestNot "Not Well Formed 38" (parseXMLSnippet) "<doc>]]></doc>"
    ,  parseTestNot "Not Well Formed 39" (parseXMLSnippet) "<doc>]]]></doc>"
    ,  parseTestNot "Not Well Formed 40" (parseXMLSnippet) "<doc> <!-- abc </doc>"
    ,  parseTestNot "Not Well Formed 41" (parseXMLSnippet) "<doc> <?a pi that is not closed </doc>"
    ,  parseTestNot "Not Well Formed 42" (parseXMLSnippet) "<doc>abc]]]>def</doc>"

    ,  parseTestNot "Not Well Formed 43" (parseXMLSnippet) "<doc><?pi a form feed (asaas) is not allowed in a pi?></doc>"
    ,  parseTestNot "Not Well Formed 44" (parseXMLSnippet) "<doc>1 < 2 but not in XML</doc>"
    ,  parseTestNot "Not Well Formed 45" (parseXMLSnippet) "<doc></doc> Illegal data"
    ,  parseTestNot "Not Well Formed 46" (parseXMLSnippet) "<doc></doc> &#32;"
    ,  parseTestNot "Not Well Formed 47" (parseXMLSnippet) "<doc x=\"foo\" y=\"bar\" x=\"baz\"></doc>"

    ,  parseTestNot "Not Well Formed 48" (parseXMLSnippet) "<doc><a></aa></doc>"
    ,  parseTestNot "Not Well Formed 49" (parseXMLSnippet) "<doc></doc> <doc></doc>"
    ,  parseTestNot "Not Well Formed 50" (parseXMLSnippet) "<doc/> <doc></doc>"
    ,  parseTestNot "Not Well Formed 51" (parseXMLSnippet) "<doc/></doc/>"
    ,  parseTestNot "Not Well Formed 52" (parseXMLSnippet) "<doc/> Illegal data"
    ,  parseTestNot "Not Well Formed 53" (parseXMLSnippet) "<doc> <a/ </doc>"
    ,  parseTestNot "Not Well Formed 54" (parseXMLSnippet) "<doc> <a/</a> </doc>"
    ,  parseTestNot "Not Well Formed 55" (parseXMLSnippet) "<doc> <a / > </doc>"
    ,  parseTestNot "Not Well Formed 56" (parseXMLSnippet) "<doc> </doc> <![CDATA[]]>"


    ,  parseTestNot "Not Well Formed 57" (parseXMLSnippet) "<doc> <a><![CDATA[xyz]]]></a> <![CDATA[]]></a> </doc>"
    ,  parseTestNot "Not Well Formed 58" (parseXMLSnippet) "<!-- a comment --> <![CDATA[]]> <doc></doc>"
    ,  parseTestNot "Not Well Formed 59" (parseXMLSnippet) "<!-- a comment --> &#32; <doc></doc>"
    ,  parseTestNot "Not Well Formed 60" (parseXMLSnippet) "<doc></DOC>"
    ,  parseTestNot "Not Well Formed 61" (parseXMLSnippet) "<!-- a comment ending with three dashes ---> <doc></doc>"
    ,  parseTestNot "Not Well Formed 62" (parseXMLSnippet) "<doc>&foo;</doc>"
    ,  parseTestNot "Not Well Formed 63" (parseXMLSnippet) "<doc a=\"&foo;\"></doc>"
    ,  parseTestNot "Not Well Formed 64" (parseXMLSnippet) "<doc>&#X58;</doc>"
    ,  parseTestNot "Not Well Formed 65" (parseXMLSnippet) "<?pi stuff?> <![CDATA[]]> <doc> </doc>"
    ,  parseTestNot "Not Well Formed 66" (parseXMLSnippet) "<?pi data?> &#32;<doc></doc>"
    ,  parseTestNot "Not Well Formed 67" (parseXMLSnippet) "<doc> <![CDATA [ ]]> </doc>"

    ,  parseTestNot "Not Well Formed 68" (parseXMLSnippet) "<doc> <![cdata[data]]> </doc>"
    ,  parseTestNot "Not Well Formed 69" (parseXMLSnippet) "<!-- --> <?xml version=\"1.0\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 70" (parseXMLSnippet) "<doc> <?xml version=\"1.0\"?> </doc>"
    ,  parseTestNot "Not Well Formed 71" (parseXMLSnippet) "<doc> </doc> <?xml version=\"1.0\"?>"
    ,  parseTestNot "Not Well Formed 72" (parseXMLSnippet) "<doc> <?xMl version=\"1.0\"?> </doc>"
    ,  parseTestNot "Not Well Formed 73" (parseXMLSnippet) "<doc> <?xmL?> </doc>"
    ,  parseTestNot "Not Well Formed 74" (parseXMLSnippet) "<doc>/doc>"
    ,  parseTestNot "Not Well Formed 75" (parseXMLSnippet) "<!--xxx --> <doc></doc>"
    ,  parseTestNot "Not Well Formed 76" (parseXMLSnippet) "<?pas ?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 77" (parseXMLSnippet) "<doc a=\"a\"></doc>"
    ,  parseTestNot "Not Well Formed 78" (parseXMLSnippet) "<doc><![CDATA[]]></doc>"
    ,  parseTestNot "Not Well Formed 79" (parseXMLSnippet) "<!DOCTYPE a [ <!ELEMENT a EMPTY> <!ATTLIST a b CDATA #IMPLIED d CDATA #IMPLIED> ]> <a b=\"c\"d=\"e\"/>"
    ]
-- --------------------------------------------------------------------- [ EOF ]
