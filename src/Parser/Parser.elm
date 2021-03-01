module Parser.Parser exposing (parseLoop, getErrors)

{-| Function parserLoop takes as input an integer representing a "chunkNumber"
and as string representing a chunk of text. It produces as output a TextCursor:

       type alias TextCursor =
           { text : String
           , chunkNumber : Int
           , parsed : List Expression
           , stack : List String
           , offset : Int
           }

parserLoop accomplishes this by initializing a TextCursor with given values of
chunkNumber and text, then repeatedly applying applying a function 'nextCursor.'
Function nextCursor runs the expression parser on the text, consuming part of the
text and producing a value of type Expression which is prepended to parsed, which
is a list of Expressions.

An Expression contains a SourceMap. It identifies the part of source
text from which it was derived.

        type alias SourceMap =
            { chunkOffset : Int, length : Int, offset : Int }

The length field of the SourceMap is added to update the offset field of the
TextCursor. In this way, the offset and length identify the source text within
a chunk of text, while the chunkOffset identifies the chunk of text within
the full text.

TO ADD: COMMENTS ON THE STACK

@docs parseLoop, getErrors


## Uses of Parser.Parser

  - parseLoop in Parser.Document
  - getErrors in Main

-}

import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Expression as Expression exposing (Expression(..), Problem(..), SourceMap)
import Parser.TextCursor as TextCursor exposing (TextCursor)


type alias Parser a =
    Parser.Parser Context Problem a


type Context
    = Foo
    | Bar


{-| parseLoop takes as input an integer representing a "chunkOffset" and
a string of source text, the "chunk." It returns a TextCursor, which
is a data structure which includes the parsed source text.
-}
parseLoop : Int -> String -> TextCursor
parseLoop initialLineNumber str =
    loop (TextCursor.init initialLineNumber str) nextCursor


nextCursor : TextCursor -> Step TextCursor TextCursor
nextCursor tc =
    if tc.text == "" then
        Done { tc | parsed = List.reverse tc.parsed }

    else
        case Parser.run (expression tc.chunkNumber) tc.text of
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


handleError : TextCursor -> List (Parser.DeadEnd Context Problem) -> TextCursor
handleError tc_ e =
    let
        -- Err [{ col = 10, contextStack = [], problem = ExpectingRightBraceForArg, row = 1 }]
        mFirstError =
            e |> List.head

        problem =
            Maybe.map .problem mFirstError |> Maybe.withDefault GenericError

        errorColumn =
            mFirstError |> Maybe.map .col |> Maybe.withDefault 0

        errorText =
            String.left errorColumn tc_.text

        newText =
            String.dropLeft errorColumn tc_.text
    in
    { text = newText
    , chunkNumber = tc_.chunkNumber
    , parsed = LXError errorText problem { chunkOffset = tc_.chunkNumber, length = errorColumn, offset = tc_.offset + errorColumn } :: tc_.parsed
    , stack = errorText :: tc_.stack
    , offset = tc_.offset + errorColumn
    }


{-|

> run expressionList "foo bar $a^2$ baz"
> Ok [Text ("foo bar "),InlineMath "a^2",Text "baz"]

-}
expressionList : Int -> Parser (List Expression)
expressionList lineNumber =
    many (expression lineNumber)


expression : Int -> Parser.Parser Context Problem Expression
expression lineNumber =
    -- Parser.oneOf [ Parser.backtrackable (bareMacro lineNumber), macro lineNumber, displayMath lineNumber, inlineMath lineNumber, text lineNumber ]
    Parser.oneOf [ macro lineNumber, displayMath lineNumber, inlineMath lineNumber, text lineNumber ]


{-| getErrors takes parsed text as input and produces a list of errors as output.
-}
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


rawTextP_ : Int -> Char -> List Char -> Parser ( String, SourceMap )
rawTextP_ lineNumber prefixChar stopChars =
    getChompedString lineNumber <|
        Parser.succeed ()
            |. Parser.chompIf (\c -> c == prefixChar) (ExpectingPrefix prefixChar)
            |. Parser.chompWhile (\c -> not (List.member c stopChars))


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


{-|

    Use this to parse a string and return information about its location in the source

-}
getChompedString : Int -> Parser a -> Parser ( String, SourceMap )
getChompedString lineNumber parser =
    Parser.succeed (\first_ last_ source_ -> ( String.slice first_ last_ source_, { chunkOffset = lineNumber, length = last_, offset = 0 } ))
        |= Parser.getOffset
        |. parser
        |= Parser.getOffset
        |= Parser.getSource


macro : Int -> Parser Expression
macro lineNo =
    Parser.succeed (\n o a -> fixMacro lineNo n o a)
        |= macroName lineNo
        |= optArg lineNo
        |= many (arg lineNo)


bareMacro : Int -> Parser Expression
bareMacro lineNo =
    Parser.succeed (\( name, sm ) -> Macro name Nothing [] sm)
        |= bareMacroName lineNo



-- oneSpace : Int -> Expression


oneSpace : Int -> Parser Expression
oneSpace lineNo =
    Parser.succeed (\offset -> LXNull () { chunkOffset = lineNo, offset = offset, length = 1 })
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token " " ExpectingSpace)


fixMacro : Int -> ( String, SourceMap ) -> Maybe ( String, SourceMap ) -> List Expression -> Expression
fixMacro lineNo ( name, sm1 ) optArg_ args_ =
    let
        lineNumber =
            sm1.chunkOffset

        offset =
            sm1.offset

        l1 =
            sm1.length

        l2 =
            Maybe.map (Tuple.second >> .length) optArg_ |> Maybe.withDefault 0

        lengths_ =
            (Expression.getSourceOfList args_).length

        sm =
            { chunkOffset = lineNumber, offset = offset, length = lengths_ + 1 }
    in
    Macro name (Maybe.map Tuple.first optArg_) args_ sm


macroName : Int -> Parser ( String, SourceMap )
macroName lineNo =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "\\" ExpectingLeadingBackslashForMacro)
        |= rawText lineNo [ '{', '[' ]


bareMacroName : Int -> Parser ( String, SourceMap )
bareMacroName lineNo =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "\\" ExpectingLeadingBackslashForMacro)
        |= rawText lineNo [ ' ' ]
        |. Parser.symbol (Parser.Token " " ExpectingSpace)


optArg : Int -> Parser (Maybe ( String, SourceMap ))
optArg lineNo =
    Parser.oneOf [ optArg__ lineNo |> Parser.map Just, Parser.succeed Nothing ]


optArg__ : Int -> Parser ( String, SourceMap )
optArg__ lineNo =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "[" ExpectingLeftBracketForOptArg)
        |= rawText lineNo [ ']' ]
        |. Parser.symbol (Parser.Token "]" ExpectingRightBracketForOptArg)


arg : Int -> Parser Expression
arg lineNo =
    Parser.succeed (\o1 e o2 -> fixArg o1 e o2)
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token "{" ExpectingLeftBraceForArg)
        |= Parser.oneOf
            [ inlineMath lineNo

            --, Parser.lazy (\_ -> Parser.oneOf [ Parser.backtrackable (bareMacro lineNo), macro lineNo ])
            , Parser.lazy (\_ -> macro lineNo)
            , text_ lineNo [ '}' ]
            ]
        |. Parser.symbol (Parser.Token "}" ExpectingRightBraceForArg)
        |= Parser.getOffset


fixArg : Int -> Expression -> Int -> Expression
fixArg k1 e k2 =
    e



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
