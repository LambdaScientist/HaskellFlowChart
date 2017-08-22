module TestArea where


import Prelude
import FlowChart
import StringTools


import Text.PrettyPrint

import Text.PrettyPrint.HughesPJClass


import Data.String.Utils

foo = While "Do this" [TitlePath ("FOO1", bar),TitlePath ("FOO2", bar),TitlePath ("FOO3", bar) ]
foo2 = If [TitlePath ("FOO1", Branch $ foo),TitlePath ("FOO2", bar),TitlePath ("FOO3", bar) ]

bar = Node "Code" 

quickFoo = replace "        " "\t" $  prettyShow foo2


writeFoo = writeFile "umlet.txt" quickFoo 