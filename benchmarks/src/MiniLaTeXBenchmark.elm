module MiniLaTeXBenchmark exposing(main)

import Benchmark.Runner exposing (BenchmarkProgram, program)



import MiniLaTeX
import Data.MiniLaTeXIO
import Data.Short
import Data.Medium
import Data.Long
import Benchmark exposing (..)

main : BenchmarkProgram
main =
      program suite

suite : Benchmark
suite =
    describe "MiniLaTeX"
        [ -- nest as many descriptions as you like
          describe "MiniLaTeX.ini"
            [ benchmark "Data.Long 1" <|
                \_ -> MiniLaTeX.initWithString 0 "foo"( Data.Long.sourceText 1)
             , benchmark "Data.Long 4" <|
                \_ -> MiniLaTeX.initWithString 0 "foo"( Data.Long.sourceText 4)
            ]
        --
        ---- compare the results of two benchmarks
        --, Benchmark.compare "initialize"
        --    "HAMT"
        --    (\_ -> Hamt.initialize 100 identity)
        --    "core"
        --    (\_ -> Array.initialize 100 identity)
        ]

