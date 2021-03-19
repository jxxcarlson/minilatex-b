module Parser.ToolAdvanced exposing (..)

import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Expression exposing (Context(..), Problem(..))


type alias Parser a =
    Parser.Parser Context Problem a


{-| Apply a parser zero or more times and return a list of the results.
-}
many : Parser a -> Parser (List a)
many p =
    Parser.loop [] (manyHelp p)


manyHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
manyHelp p vs =
    Parser.oneOf
        [ Parser.succeed (\v -> Parser.Loop (v :: vs))
            |= p
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        ]


{-| Running `optional p` means run p, but if it fails, succeed anyway
-}
optional : Parser () -> Parser ()
optional p =
    Parser.oneOf [ p, Parser.succeed () ]


{-| running `second p q` means run p, then run q
and return the result of running q.
-}
second : Parser a -> Parser b -> Parser b
second p q =
    p |> Parser.andThen (\_ -> q)


{-| textPS = "text prefixText stopCharacters": Get the longest string
whose first character satisfies the prefixTest and whose remaining
characters are not in the list of stop characters. Example:

    line =
        textPS (\c -> Char.isAlpha) [ '\n' ]

recognizes lines that start with an alphabetic character.

-}
textPS : (Char -> Bool) -> List Char -> Parser { start : Int, finish : Int, content : String }
textPS prefixTest stopChars =
    Parser.succeed (\start finish content -> { start = start, finish = finish, content = String.left (finish - start) content })
        |= Parser.getOffset
        |. Parser.chompIf (\c -> prefixTest c) UnHandledError
        |. Parser.chompWhile (\c -> not (List.member c stopChars))
        |= Parser.getOffset
        |= Parser.getSource
