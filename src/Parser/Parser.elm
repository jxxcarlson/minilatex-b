module Parser.Parser exposing
    ( arg
    , expression
    , expressionList
    , getErrors
    , macro
    , macroName
    , many
    , optArg
    , optArg__
    , parseLoop
    )

import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Expression as Expression exposing (Expression(..), Problem(..), SourceMap)
import Parser.TextCursor as TextCursor exposing (TextCursor)


type alias Parser a =
    Parser.Parser Context Problem a


type Context
    = Foo
    | Bar


parseLoop : Int -> String -> TextCursor
parseLoop initialLineNumber str =
    loop (TextCursor.init initialLineNumber str) nextRound


nextRound : TextCursor -> Step TextCursor TextCursor
nextRound tc =
    let
        _ =
            Debug.log "TXT" tc.text
    in
    if tc.text == "" then
        Done { tc | parsed = List.reverse tc.parsed }

    else
        case Parser.run (expression tc.lineNumber) tc.text of
            Ok expr ->
                let
                    sourceMap =
                        Expression.getSource expr

                    newText =
                        String.dropLeft sourceMap.length tc.text

                    newExpr =
                        Expression.incrementOffset tc.offset expr
                in
                Loop { tc | text = newText, parsed = newExpr :: tc.parsed, offset = tc.offset + sourceMap.length }

            Err e ->
                Loop (handleError tc e)


handleError tc_ e =
    let
        mFirstError =
            e |> List.head

        problem =
            Maybe.map .problem mFirstError |> Maybe.withDefault GenericError

        errorColumn =
            mFirstError |> Maybe.map .col |> Maybe.withDefault 0

        errorText =
            String.left errorColumn tc_.text

        errorColumn2 =
            case Parser.run (word tc_.lineNumber) errorText of
                Ok ( _, t ) ->
                    t.length

                Err err ->
                    4

        errorText2 =
            String.left errorColumn2 errorText

        newText =
            String.dropLeft errorColumn2 tc_.text
    in
    { text = newText
    , lineNumber = tc_.lineNumber
    , parsed = LXError errorText2 problem { lineNumber = tc_.lineNumber, length = errorColumn2, offset = tc_.offset + errorColumn2 } :: tc_.parsed
    , stack = errorText :: tc_.stack
    , offset = tc_.offset + errorColumn
    }


{-|

    Use this to parse a string and return information about its location in the source

-}
getChompedString : Int -> Parser a -> Parser ( String, SourceMap )
getChompedString lineNumber parser =
    Parser.succeed (\first_ last_ source_ -> ( String.slice first_ last_ source_, { lineNumber = lineNumber, length = last_, offset = 0 } ))
        |= Parser.getOffset
        |. parser
        |= Parser.getOffset
        |= Parser.getSource


{-|

> run expressionList "foo bar $a^2$ baz"
> Ok [Text ("foo bar "),InlineMath "a^2",Text "baz"]

-}
expressionList : Int -> Parser (List Expression)
expressionList lineNumber =
    many (expression lineNumber)


expression : Int -> Parser.Parser Context Problem Expression
expression lineNumber =
    Parser.oneOf [ macro lineNumber, displayMath lineNumber, inlineMath lineNumber, text lineNumber ]


getErrors : List (List Expression) -> List Expression
getErrors list =
    list
        |> List.map getErrors_
        |> List.concat


getErrors_ : List Expression -> List Expression
getErrors_ list =
    List.filter (\e -> isError e) list


isError : Expression -> Bool
isError expr =
    case expr of
        LXError _ _ _ ->
            True

        _ ->
            False



-- TEXT


text : Int -> Parser Expression
text lineNumber =
    text_ lineNumber [ '$', '\\' ]


text_ : Int -> List Char -> Parser Expression
text_ lineNumber stopChars =
    Parser.map (\( t, s ) -> Text t s) (rawText lineNumber stopChars)


rawText : Int -> List Char -> Parser ( String, SourceMap )
rawText lineNumber stopChars =
    getChompedString lineNumber <|
        Parser.succeed ()
            |. Parser.chompWhile (\c -> not (List.member c stopChars))



-- MATH


inlineMath : Int -> Parser Expression
inlineMath lineNumber =
    Parser.succeed (\( s, t ) -> InlineMath s { t | length = t.length + 1 })
        |. Parser.symbol (Parser.Token "$" ExpectingLeadingDollarSign)
        |= getChompedString lineNumber (Parser.chompUntil (Parser.Token "$" ExpectingTrailingDollarSign1))
        |. Parser.symbol (Parser.Token "$" ExpectingTrailingDollarSign2)
        |. Parser.spaces


displayMath : Int -> Parser Expression
displayMath lineNumber =
    Parser.succeed (\( s, t ) -> DisplayMath s { t | length = t.length + 2 })
        |. Parser.symbol (Parser.Token "$$" ExpectingLeadingDoubleDollarSign)
        |= getChompedString lineNumber (Parser.chompUntil (Parser.Token "$$" ExpectingLTrailingDoubleDollarSign1))
        |. Parser.symbol (Parser.Token "$$" ExpectingLTrailingDoubleDollarSign2)
        |. Parser.spaces



-- MACRO


macro : Int -> Parser Expression
macro lineNo =
    Parser.succeed (\n o a -> fixMacro n o a)
        |= macroName lineNo
        |= optArg lineNo
        |= many (arg lineNo)


fixMacro : ( String, SourceMap ) -> Maybe ( String, SourceMap ) -> List ( String, SourceMap ) -> Expression
fixMacro ( name, sm1 ) optArg_ args_ =
    let
        lineNumber =
            sm1.lineNumber

        offset =
            sm1.offset

        l1 =
            sm1.length

        l2 =
            Maybe.map (Tuple.second >> .length) optArg_ |> Maybe.withDefault 0

        realArgs =
            List.map Tuple.first (Debug.log "ARGS" args_)

        lengths_ =
            List.map (Tuple.second >> .length) args_ |> List.sum

        length =
            l1 + l2 + lengths_

        sm =
            { lineNumber = lineNumber, offset = offset, length = length }
    in
    Macro name (Maybe.map Tuple.first optArg_) realArgs sm


macroName : Int -> Parser ( String, SourceMap )
macroName lineNo =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "\\" ExpectingLeadingBackslashForMacro)
        |= rawText lineNo [ '{', '[' ]


optArg : Int -> Parser (Maybe ( String, SourceMap ))
optArg lineNo =
    Parser.oneOf [ optArg__ lineNo |> Parser.map Just, Parser.succeed Nothing ]


optArg__ : Int -> Parser ( String, SourceMap )
optArg__ lineNo =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "[" ExpectingLeftBracketForOptArg)
        |= rawText lineNo [ ']' ]
        |. Parser.symbol (Parser.Token "]" ExpectingRightBracketForOptArg)


arg : Int -> Parser ( String, SourceMap )
arg lineNo =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "{" ExpectingLeftBraceForArg)
        |= rawText lineNo [ '}' ]
        |. Parser.symbol (Parser.Token "}" ExpectingRightBraceForArg)



-- TOOLS


word : Int -> Parser ( String, SourceMap )
word lineNumber =
    Parser.succeed identity
        |= getChompedString lineNumber (Parser.chompUntil (Parser.Token " " ExpectingEndOfWordSpace))


loop : state -> (state -> Step state a) -> a
loop s nextState =
    case nextState s of
        Loop s_ ->
            loop s_ nextState

        Done b ->
            b


type Step state a
    = Loop state
    | Done a


{-| Apply a parser zero or more times and return a list of the results.
-}
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
    Parser.end EndOfInput



-- ROUND TRIP TESTING
