module ParserToolTest exposing (suite)

import Expect
import Parser exposing ((|.), (|=), Parser, run)
import Parser.Tool
import Test exposing (describe, test)


suite =
    describe "The ParserTool module"
        [ describe "many"
            -- Nest as many descriptions as you like.
            [ test "many int2" <|
                \_ ->
                    "1 2 3"
                        |> run (Parser.Tool.many int2)
                        |> Expect.equal (Ok [ 1, 2, 3 ])
            , test "many int3" <|
                \_ ->
                    "i: 1, i: 2, i: 3"
                        |> run (Parser.Tool.many int3)
                        |> Expect.equal (Ok [ 1, 2, 3 ])
            , test "many int4" <|
                \_ ->
                    "i: 1, i: 2, i: 3"
                        |> run (Parser.Tool.many int3)
                        |> Expect.equal (Ok [ 1, 2, 3 ])
            , test "many int4 with empty input" <|
                \_ ->
                    ""
                        |> run (Parser.Tool.many int3)
                        |> Expect.equal (Ok [])
            ]
        , describe "textPS" <|
            [ test "line" <|
                \_ ->
                    "One two three!\nHo ho ho!"
                        |> run (Parser.Tool.textPS (\c -> Char.isAlpha c) [ '\n' ])
                        |> Expect.equal (Ok { start = 0, finish = 14, content = "One two three!" })
            ]
        ]


int2 : Parser Int
int2 =
    Parser.succeed identity
        |= Parser.int
        |. Parser.spaces


int3 : Parser Int
int3 =
    Parser.succeed identity
        |. Parser.symbol "i: "
        |= Parser.int
        |. Parser.oneOf [ Parser.symbol ",", Parser.succeed () ]
        |. Parser.spaces


{-| int4 = int3
-}
int4 : Parser Int
int4 =
    Parser.succeed identity
        |. Parser.symbol "i: "
        |= Parser.int
        |. suffix2


suffix : Parser ()
suffix =
    Parser.andThen (\_ -> Parser.spaces) (Parser.oneOf [ Parser.symbol ",", Parser.succeed () ])


suffix2 : Parser ()
suffix2 =
    Parser.Tool.second (Parser.Tool.optional (Parser.symbol ",")) Parser.spaces