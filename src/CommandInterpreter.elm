module CommandInterpreter exposing (Command, get, getIntArg)

import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Result


type alias Command =
    { name : String, args : List String }


get : String -> Maybe Command
get str =
    Parser.run commandParser str |> Result.toMaybe


getIntArg : Int -> List String -> Int
getIntArg k args =
    List.Extra.getAt 0 args |> Maybe.andThen String.toInt |> Maybe.withDefault 0


commandParser : Parser Command
commandParser =
    Parser.succeed Command
        |= word
        |= many word


word : Parser String
word =
    Parser.succeed identity
        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' '))
        |. Parser.spaces


many : Parser a -> Parser (List a)
many p =
    Parser.loop [] (manyHelp p)


manyHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
manyHelp p vs =
    Parser.oneOf
        [ eof |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        , Parser.succeed (\v -> Parser.Loop (v :: vs))
            |= p
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        ]


eof : Parser ()
eof =
    Parser.end
