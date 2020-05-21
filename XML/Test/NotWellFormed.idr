-- ------------------------------------------------------- [ NotWellFormed.idr ]
-- Module    : NotWellFormed.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Tests taken from [here](http://www.jclark.com/xml/).
module XML.Test.NotWellFormed

import XML.DOM
import XML.Serialise

import Test.Unit

parseTestNot : String
            -> (String -> Either XMLError (Document a))
            -> String
            -> IO Bool
parseTestNot title p i = genericTest (Just title) (isLeft $ p i) True (==)

export
runTests : IO ()
runTests = do
  putStrLn "=> Not WellFormed"

  runTests [
       parseTestNot "Not Well Formed 1" (Doc.fromString) "<?xml VERSION=\"1.0\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 2" (Doc.fromString) "<?xml encoding=\"UTF-8\" version=\"1.0\"?> <doc></doc>"

    ,  parseTestNot "Not Well Formed 3" (Doc.fromString) "<?xml version=\"1.0\" encoding=\"UTF-8\" ?> <doc></doc>"

    ,  parseTestNot "Not Well Formed 4" (Doc.fromString) "<?xml version=\"1.0' encoding=\"UTF-8\" ?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 5" (Doc.fromString) "<?xml version=\"1.0\" version=\"1.0\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 6" (Doc.fromString) "<?xml version=\"1.0\" valid=\"no\" ?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 7" (Doc.fromString) "<?xml version=\"1.0\" standalone=\"YES\" ?> <doc></doc>"

    ,  parseTestNot "Not Well Formed 8" (Doc.fromString) "<?xml version=\"1.0\" encoding=\" UTF-8\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 9" (Doc.fromString) "<?xml version=\"1.0 \" ?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 10" (Doc.fromString) "<?xml version=\"1.0\"?> <doc></doc>"

    ,  parseTestNot "Not Well Formed 11" (Doc.fromString) "<?xml encoding=\"UTF-8\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 12" (Doc.fromString) "<?XML version=\"1.0\"?> <doc></doc>"
    ,  parseTestNot "Not Well Formed 13" (Doc.fromString) "<?xmL version=\"1.0\"?> <doc></doc>"

    ,  parseTestNot "Not Well Formed 14" (Snippet.fromString) "<doc> <doc ? <a</a> </doc>"
    ,  parseTestNot "Not Well Formed 15" (Snippet.fromString) "<doc> <.doc></.doc> </doc>"
    ,  parseTestNot "Not Well Formed 16" (Snippet.fromString) "<doc><? ?></doc>"
    ,  parseTestNot "Not Well Formed 17" (Snippet.fromString) "<doc><?target some data></doc>"
    ,  parseTestNot "Not Well Formed 18" (Snippet.fromString) "<doc><?target some data?</doc>"

    ,  parseTestNot "Not Well Formed 19" (Snippet.fromString) "<doc><!-- a comment -- another --></doc>"

    ,  parseTestNot "Not Well Formed 20" (Snippet.fromString) "<doc>&amp no refc</doc>"
    ,  parseTestNot "Not Well Formed 21" (Snippet.fromString) "<doc>&.entity;</doc>"
    ,  parseTestNot "Not Well Formed 22" (Snippet.fromString) "<doc>&#RE;</doc>"
    ,  parseTestNot "Not Well Formed 23" (Snippet.fromString) "<doc>A & B</doc>"

    ,  parseTestNot "Not Well Formed 24" (Snippet.fromString) "<doc a1></doc>"
    ,  parseTestNot "Not Well Formed 25" (Snippet.fromString) "<doc a1=v1></doc>"
    ,  parseTestNot "Not Well Formed 26" (Snippet.fromString) "<doc a1=\"v1'></doc>"

    ,  parseTestNot "Not Well Formed 27" (Snippet.fromString) "<doc a1=\"<foo>\"></doc>"

    ,  parseTestNot "Not Well Formed 28" (Snippet.fromString) "<doc a1=></doc>"
    ,  parseTestNot "Not Well Formed 29" (Snippet.fromString) "<doc a1=\"v1\" \"v2\"></doc>"
    ,  parseTestNot "Not Well Formed 30" (Snippet.fromString) "<doc><![CDATA[</doc>"
    ,  parseTestNot "Not Well Formed 31" (Snippet.fromString) "<doc><![CDATA [ stuff]]></doc>"
    ,  parseTestNot "Not Well Formed 32" (Snippet.fromString) "<doc></>"

    ,  parseTestNot "Not Well Formed 33" (Snippet.fromString) "<doc a1=\"A & B\"></doc>"
    ,  parseTestNot "Not Well Formed 34" (Snippet.fromString) "<doc a1=\"a&b\"></doc>"
    ,  parseTestNot "Not Well Formed 35" (Snippet.fromString) "<doc a1=\"&#123:\"></doc>"
    ,  parseTestNot "Not Well Formed 36" (Snippet.fromString) "<doc 12=\"34\"></doc>"

    ,  parseTestNot "Not Well Formed 37" (Snippet.fromString) "<doc> <123></123> </doc>"
    ,  parseTestNot "Not Well Formed 38" (Snippet.fromString) "<doc>]]></doc>"
    ,  parseTestNot "Not Well Formed 39" (Snippet.fromString) "<doc>]]]></doc>"
    ,  parseTestNot "Not Well Formed 40" (Snippet.fromString) "<doc> <!-- abc </doc>"
    ,  parseTestNot "Not Well Formed 41" (Snippet.fromString) "<doc> <?a pi that is not closed </doc>"
    ,  parseTestNot "Not Well Formed 42" (Snippet.fromString) "<doc>abc]]]>def</doc>"

    ,  parseTestNot "Not Well Formed 43" (Snippet.fromString) "<doc><?pi a form feed (asaas) is not allowed in a pi?></doc>"
    ,  parseTestNot "Not Well Formed 44" (Snippet.fromString) "<doc>1 < 2 but not in XML</doc>"

    ,  parseTestNot "Not Well Formed 45" (Snippet.fromString) "<doc></doc> Illegal data"
    ,  parseTestNot "Not Well Formed 46" (Snippet.fromString) "<doc></doc> &#32;"
    ,  parseTestNot "Not Well Formed 47" (Snippet.fromString) "<doc x=\"foo\" y=\"bar\" x=\"baz\"></doc>"
    ,  parseTestNot "Not Well Formed 48" (Snippet.fromString) "<doc><a></aa></doc>"

    ,  parseTestNot "Not Well Formed 49" (Snippet.fromString) "<doc></doc> <doc></doc>"

    ,  parseTestNot "Not Well Formed 50" (Snippet.fromString) "<doc/> <doc></doc>"
    ,  parseTestNot "Not Well Formed 51" (Snippet.fromString) "<doc/></doc/>"
    ,  parseTestNot "Not Well Formed 52" (Snippet.fromString) "<doc/> Illegal data"
    ,  parseTestNot "Not Well Formed 53" (Snippet.fromString) "<doc> <a/ </doc>"

    ,  parseTestNot "Not Well Formed 54" (Snippet.fromString) "<doc> <a/</a> </doc>"

    ,  parseTestNot "Not Well Formed 55" (Snippet.fromString) "<doc> <a / > </doc>"
    ,  parseTestNot "Not Well Formed 56" (Snippet.fromString) "<doc> </doc> <![CDATA[]]>"


    ,  parseTestNot "Not Well Formed 57" (Snippet.fromString) "<doc> <a><![CDATA[xyz]]]></a> <![CDATA[]]></a> </doc>"
    ,  parseTestNot "Not Well Formed 58" (Snippet.fromString) "<!-- a comment --> <![CDATA[]]> <doc></doc>"
    ,  parseTestNot "Not Well Formed 59" (Snippet.fromString) "<!-- a comment --> &#32; <doc></doc>"
    ,  parseTestNot "Not Well Formed 60" (Snippet.fromString) "<doc></DOC>"
    ,  parseTestNot "Not Well Formed 61" (Snippet.fromString) "<!-- a comment ending with three dashes ---> <doc></doc>"
    ,  parseTestNot "Not Well Formed 62" (Snippet.fromString) "<doc>&foo;</doc>"
    ,  parseTestNot "Not Well Formed 63" (Snippet.fromString) "<doc a=\"&foo;\"></doc>"
    ,  parseTestNot "Not Well Formed 64" (Snippet.fromString) "<doc>&#X58;</doc>"
    ,  parseTestNot "Not Well Formed 65" (Snippet.fromString) "<?pi stuff?> <![CDATA[]]> <doc> </doc>"
    ,  parseTestNot "Not Well Formed 66" (Snippet.fromString) "<?pi data?> &#32;<doc></doc>"
    ,  parseTestNot "Not Well Formed 67" (Snippet.fromString) "<doc> <![CDATA [ ]]> </doc>"

    ,  parseTestNot "Not Well Formed 68" (Snippet.fromString) "<doc> <![cdata[data]]> </doc>"
    ,  parseTestNot "Not Well Formed 69" (Snippet.fromString) "<!-- --> <?xml version=\"1.0\"?> <doc></doc>"

    ,  parseTestNot "Not Well Formed 70" (Snippet.fromString) "<doc> <?xml version=\"1.0\"?> </doc>"
    ,  parseTestNot "Not Well Formed 71" (Snippet.fromString) "<doc> </doc> <?xml version=\"1.0\"?>"
    ,  parseTestNot "Not Well Formed 72" (Snippet.fromString) "<doc> <?xMl version=\"1.0\"?> </doc>"

    ,  parseTestNot "Not Well Formed 73" (Snippet.fromString) "<doc> <?xmL?> </doc>"
    ,  parseTestNot "Not Well Formed 74" (Snippet.fromString) "<doc>/doc>"
    ,  parseTestNot "Not Well Formed 75" (Snippet.fromString) "<!--xxx --> <doc></doc>"
    ,  parseTestNot "Not Well Formed 76" (Snippet.fromString) "<?pas ?> <doc></doc>"

    ,  parseTestNot "Not Well Formed 77" (Snippet.fromString) "<doc a=\"a\"></doc>"
    ,  parseTestNot "Not Well Formed 78" (Snippet.fromString) "<doc><![CDATA[]]></doc>"

    ,  parseTestNot "Not Well Formed 79" (Snippet.fromString) "<!DOCTYPE a [ <!ELEMENT a EMPTY> <!ATTLIST a b CDATA #IMPLIED d CDATA #IMPLIED> ]> <a b=\"c\"d=\"e\"/>"
    ]
-- --------------------------------------------------------------------- [ EOF ]
