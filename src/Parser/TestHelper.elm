module Parser.TestHelper exposing (parseAndRecompose, roundTrip, roundTripCheck, squeezeSpace)

import Parser.Advanced as Parser
import Parser.Expression as Expression exposing (Expression)
import Parser.Parser as LXParser


toStringFromList : List Expression -> String
toStringFromList list =
    list
        |> List.map Expression.toString
        |> String.join " "


parseAndRecompose : String -> String
parseAndRecompose str =
    str |> LXParser.parseLoop 0 |> .parsed |> toStringFromList


roundTrip : String -> String
roundTrip str =
    case Parser.run (LXParser.expressionList 0) str of
        Ok r ->
            List.map Expression.toString r |> String.join " "

        Err err ->
            Debug.toString err


squeeze : String -> String
squeeze str =
    str |> String.replace " " "" |> String.replace "\n" ""


squeezeSpace : String -> String
squeezeSpace str =
    str |> String.replace " " ""


roundTripCheck : String -> Bool
roundTripCheck str =
    squeeze str == squeeze (roundTrip str)
