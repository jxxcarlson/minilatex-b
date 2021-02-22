module ParserTest exposing(suite)

import Test exposing(describe, test, fuzz)
import Expect
import Fuzz exposing(string)
import Parser.Parser exposing(..)
import Parser.Test exposing(roundTripCheck, parseAndRecompose, squeezeSpace)
import Parser.Expression exposing(..)
import Parser.Advanced as Parser exposing(run)


suite =
    describe "The Parser module"
        [ describe "expressionList" -- Nest as many descriptions as you like.
            [  test "roundTripCheck 1" <|
                                \_ ->
                                    "foo bar $a^2$ baz"
                                      |> roundTripCheck
                                      |> Expect.equal True

                , test "roundTripCheck 2" <|
                                \_ ->
                                    "foo bar $a^2$ baz $$b^2$$ yada"
                                      |> roundTripCheck
                                      |> Expect.equal True

                , test "parseAndRecompose" <|
                                \_ ->
                                    "foo bar $a^2$ baz $$b^2$$ yada"
                                      |> parseAndRecompose
                                      |> Expect.equal "foo bar  $a^2$  baz  $$b^2$$  yada"

                , test "parseAndRecompose 2" <|
                                \_ ->
                                    "foo bar $a^2$ baz\n$$b^2$$\nyada"
                                      |> parseAndRecompose |> squeezeSpace
                                      |> Expect.equal ("foo bar $a^2$ baz\n$$b^2$$\nyada" |> squeezeSpace)

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]