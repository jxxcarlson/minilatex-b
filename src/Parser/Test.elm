module Parser.Test exposing (roundTrip, roundTripCheck,squeezeSpace,  parseAndRecompose)

import Parser.Advanced as Parser
import Parser.Parser as LParser
import Parser.Expression as LExpression exposing(Expression)


toStringFromList : List Expression -> String
toStringFromList list =
    list
      |> List.map LExpression.toString
      |> String.join " "

parseAndRecompose : String -> String
parseAndRecompose str =
    str |> LParser.parseLoop 0 |> .parsed |> toStringFromList

roundTrip : String -> String
roundTrip str =
    case Parser.run (LParser.expressionList 0) str of
        Ok r -> List.map LExpression.toString r |> String.join " "
        Err err -> Debug.toString err


squeeze : String -> String
squeeze str =
    str |> String.replace " " "" |> String.replace "\n" ""

squeezeSpace : String -> String
squeezeSpace str =
    str |> String.replace " " ""

roundTripCheck : String -> Bool
roundTripCheck str =
    squeeze str == squeeze (roundTrip str)
