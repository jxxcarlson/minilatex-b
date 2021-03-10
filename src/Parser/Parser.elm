module Parser.Parser exposing
    ( Parser, Context(..)
    , parseLoop, expression, expressionList, parseExpression, macro
    )

{-| Function parserLoop takes as input an integer representing a "chunkNumber"
and as string representing a chunk of text. It produces as output a TextCursor:

       type alias TextCursor =
           { text : String
           , chunkNumber : Int
           , parsed : List Expression
           , stack : List String
           , offset : Int
           }

parserLoop accomplishes this by initializing a TextCursor with the given values of
chunkNumber and text, then repeatedly applying applying a function 'nextCursor'
until the text is exhausted. Function nextCursor runs the expression parser on the
text, consuming part of the text and producing a value of type Expression which
is prepended to 'parsed', which is a list of Expressions.

An Expression contains a SourceMap. It identifies the part of source
text from which it was derived and contains a copy of that source text.

        type alias SourceMap =
            { chunkOffset : Int, length : Int, offset : Int, content: String }

The length field of the SourceMap is added to update the offset field of the
TextCursor. In this way, the offset and length identify the source text within
a chunk of text, while the chunkOffset identifies the chunk of text within
the full text. The actual snippet of source text is used in error reporting.

The accuracy of the computation of the length of the text in each round of
parsing is important. The input text is truncated by that amount; in the
next round the truncated text is parsed.

TO ADD: COMMENTS ON THE STACK


## Types

@docs Parser, Context


## Functions

@docs parseLoop, expression, expressionList, parseExpression, macro


## Uses of Parser.Parser

  - parseLoop in Parser.Document
  - getErrors in Main

-}

----| Table Contents
----| PARSELOOP
----| EXPRESSION PARSER
----| Text
----| Math
----| Macro
----| OptArg
----| Arg
----| Environment
----| SOURCE MAP
----| HELPERS
----| Loop
----| Many
----| Many2
----| ItemList
----|
----| ( parseLoop, getErrors)
--

import Dict
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Expression as Expression exposing (Expression(..), Problem(..), SourceMap)
import Parser.Problem exposing (RecoveryData)
import Parser.TextCursor as TextCursor exposing (TextCursor)
import Set


{-| -}
type alias Parser a =
    Parser.Parser Context Problem a


{-| -}
type Context
    = InlineMathContext
    | DisplayMathContext
    | MacroNameContext
    | OptArgContext
    | ArgContext
    | WordContext
    | EnvNameContext



-- PARSELOOP


{-| parseLoop takes as input an integer representing a "chunkOffset" and
a string of source text, the "chunk." It returns a TextCursor, which
is a data structure which includes the parsed source text.
-}
parseLoop : Int -> Int -> String -> TextCursor
parseLoop generation initialLineNumber str =
    loop (TextCursor.init generation initialLineNumber str) nextCursor


{-| nextCursor operates by running the expression parser on
`tc.text` with argument `tc.chunkNumber`. This argument is used
to track the location in the source text of the piece of text
parsed.

Recall that parseLoop is fed chunks of text by
Document.process. These chunks are "logical paragraphs,"
which one may think of as forming an array of strings indexed
by chunkNumber. A piece of text within a chunk is identified
by an offset and a length:

    piece =
        String.slice offset (offset + length) chunk

If parsing succeeds, resulting in a parsand `expr`, the textCursor
operated by parseLoop is updated:

    - the "program counter" tc.count is incremented
    - the piece of text corresponding to the parsand
      is removed from tc.text
    - `expr` is prepended to `tc.parsed`

-}
nextCursor : TextCursor -> Step TextCursor TextCursor
nextCursor tc =
    if tc.text == "" || tc.count > 10 then
        -- TODO: that usage of count needs to be removed after bug is fixed
        Done { tc | parsed = List.reverse tc.parsed }

    else
        case Parser.run (expression tc.generation tc.blockIndex) tc.text of
            Ok expr ->
                let
                    sourceMap =
                        Expression.getSource expr
                in
                Loop
                    { tc
                        | count = tc.count + 1
                        , text = String.dropLeft sourceMap.length tc.text
                        , block = tc.block ++ String.left sourceMap.length tc.text
                        , parsed = newExpr tc expr :: tc.parsed
                        , offset = tc.offset + sourceMap.length
                    }

            Err e ->
                Loop (handleError tc e)


newExpr tc_ expr =
    case List.head tc_.stack of
        Just "highlight" ->
            Expression.incrementOffset tc_.offset (highlight expr (Expression.getSource expr))

        _ ->
            Expression.incrementOffset tc_.offset expr


highlight : Expression -> SourceMap -> Expression
highlight expr_ sm =
    Macro "blue" Nothing [ expr_ ] sm


handleError : TextCursor -> List (Parser.DeadEnd Context Problem) -> TextCursor
handleError tc_ e =
    let
        mFirstError =
            e |> List.head

        problem =
            mFirstError |> Maybe.map .problem |> Maybe.withDefault GenericError

        errorColumn =
            mFirstError |> Maybe.map .col |> Maybe.withDefault 0

        errorText =
            String.left errorColumn tc_.text

        mRecoveryData : Maybe RecoveryData
        mRecoveryData =
            Parser.Problem.getRecoveryData tc_ problem

        lxError =
            LXError errorText problem { content = errorText, blockOffset = tc_.blockIndex, length = errorColumn, offset = tc_.offset + errorColumn, generation = tc_.generation }
    in
    { text = makeNewText tc_ errorColumn mRecoveryData
    , block = "?? TO DO"
    , blockIndex = tc_.blockIndex
    , parsed = newParsed tc_ lxError mRecoveryData
    , stack = newStack tc_ errorText mRecoveryData
    , offset = newOffset tc_ errorColumn mRecoveryData
    , count = 0
    , generation = tc_.generation
    }


newOffset tc_ errorColumn_ mRecoveryData_ =
    case mRecoveryData_ of
        Just rd ->
            tc_.offset + rd.deltaOffset

        Nothing ->
            tc_.offset + errorColumn_


newParsed tc_ lxError_ mRecoveryData =
    case mRecoveryData of
        Just rd ->
            rd.parseSubstitute :: tc_.parsed

        _ ->
            lxError_ :: tc_.parsed


makeNewText tc_ errorColumn_ mRecoveryData =
    case mRecoveryData of
        Just rd ->
            String.dropLeft rd.textTruncation tc_.text

        Nothing ->
            String.dropLeft errorColumn_ tc_.text


newStack tc_ errorText_ mRecoveryData =
    case mRecoveryData of
        Just _ ->
            "highlight" :: tc_.stack

        Nothing ->
            errorText_ :: "highlight" :: tc_.stack



-- EXPRESSION PARSER


{-|

> run expressionList "foo bar $a^2$ baz"
> Ok [Text ("foo bar "),InlineMath "a^2",Text "baz"]

-}
expressionList : Int -> Int -> Parser (List Expression)
expressionList generation lineNumber =
    many (expression generation lineNumber)


{-| -}
parseExpression : Int -> Int -> String -> Maybe Expression
parseExpression generation lineNumber str =
    case Parser.run (expression generation lineNumber) str of
        Ok e ->
            Just e

        Err _ ->
            Nothing


{-| -}
expression : Int -> Int -> Parser Expression
expression generation lineNumber =
    Parser.oneOf
        [ macro generation lineNumber
        , environment generation lineNumber
        , displayMath generation lineNumber
        , inlineMath generation lineNumber
        , text generation lineNumber
        ]



-- Text


text : Int -> Int -> Parser Expression
text generation lineNumber =
    text_ generation lineNumber [ '$', '\\' ]


rawTextP_ : Int -> Int -> Char -> List Char -> Parser ( String, SourceMap )
rawTextP_ generation lineNumber prefixChar stopChars =
    getChompedString generation lineNumber <|
        Parser.succeed ()
            |. Parser.chompIf (\c -> c == prefixChar) (ExpectingPrefix prefixChar)
            |. Parser.chompWhile (\c -> not (List.member c stopChars))


textNP : Int -> Int -> List Char -> List Char -> Parser Expression
textNP generation lineNumber prefixChars stopChars =
    let
        sm : Int -> Int -> String -> Expression
        sm start end src =
            let
                content =
                    String.slice start end src

                sm_ =
                    { content = content
                    , length = end - start
                    , blockOffset = lineNumber
                    , offset = start
                    , generation = generation
                    }
            in
            Text content sm_
    in
    Parser.succeed sm
        |= Parser.getOffset
        |. Parser.chompIf (\c -> not (List.member c prefixChars)) (ExpectingPrefixes prefixChars)
        |. Parser.chompWhile (\c -> not (List.member c stopChars))
        |= Parser.getOffset
        |= Parser.getSource


text_ : Int -> Int -> List Char -> Parser Expression
text_ generation lineNumber stopChars =
    Parser.map (\( t, s ) -> Text t s) (rawText generation lineNumber stopChars)


rawText : Int -> Int -> List Char -> Parser ( String, SourceMap )
rawText gneration lineNumber stopChars =
    getChompedString gneration lineNumber <|
        Parser.succeed ()
            |. Parser.chompWhile (\c -> not (List.member c stopChars))


{-| Use `inWord` to parse a word.

import Parser

inWord : Char -> Bool
inWord c = not (c == ' ')

LXParser.run word "this is a test"
--> Ok "this"

-}
word_ : Problem -> (Char -> Bool) -> Parser String
word_ problem inWord =
    Parser.succeed String.slice
        |. Parser.spaces
        |= Parser.getOffset
        |. Parser.chompIf inWord problem
        |. Parser.chompWhile inWord
        |. Parser.spaces
        |= Parser.getOffset
        |= Parser.getSource


word : Int -> Int -> Parser ( String, SourceMap )
word generation lineNumber =
    Parser.inContext WordContext <|
        Parser.succeed identity
            |= getChompedString generation lineNumber (Parser.chompUntil (Parser.Token " " ExpectingEndOfWordSpace))


inOptionArgWord : Char -> Bool
inOptionArgWord c =
    not (c == '\\' || c == '$' || c == ']' || c == ' ' || c == '\n')



-- Math


inlineMath : Int -> Int -> Parser Expression
inlineMath generation lineNumber =
    Parser.inContext InlineMathContext <|
        Parser.succeed (\( s, t ) -> InlineMath s { t | length = t.length + 1 })
            |. Parser.symbol (Parser.Token "$" ExpectingLeadingDollarSign)
            |= getChompedString generation lineNumber (Parser.chompUntil (Parser.Token "$" ExpectingTrailingDollarSign))
            |. Parser.symbol (Parser.Token "$" ExpectingTrailingDollarSign)
            |. Parser.spaces


displayMath : Int -> Int -> Parser Expression
displayMath generation lineNumber =
    Parser.inContext DisplayMathContext <|
        Parser.succeed (\( s, t ) -> DisplayMath s { t | length = t.length + 2 })
            |. Parser.symbol (Parser.Token "$$" ExpectingLeadingDoubleDollarSign)
            |= getChompedString generation lineNumber (Parser.chompUntil (Parser.Token "$$" ExpectingTrailingDoubleDollarSign))
            |. Parser.symbol (Parser.Token "$$" ExpectingTrailingDoubleDollarSign)
            |. Parser.spaces



-- Macro


{-|

    Use this to parse a string and return information about its location in the source

-}
getChompedString : Int -> Int -> Parser a -> Parser ( String, SourceMap )
getChompedString generation lineNumber parser =
    let
        sm first_ last_ source_ =
            let
                src =
                    String.slice first_ last_ source_
            in
            ( src, { content = src, blockOffset = lineNumber, length = last_, offset = 0, generation = generation } )
    in
    Parser.succeed sm
        |= Parser.getOffset
        |. parser
        |= Parser.getOffset
        |= Parser.getSource


chompExpression : Int -> Int -> Parser a -> Parser ( a, SourceMap )
chompExpression generation lineNumber parser =
    let
        sm first_ expr last_ source_ =
            let
                src =
                    String.slice first_ last_ source_
            in
            ( expr, { content = src, blockOffset = lineNumber, length = last_, offset = 0, generation = generation } )
    in
    Parser.succeed sm
        |= Parser.getOffset
        |= parser
        |= Parser.getOffset
        |= Parser.getSource


{-| -}
macro : Int -> Int -> Parser Expression
macro generation lineNo =
    Parser.succeed (\start n o a end src_ -> fixMacro generation start lineNo n o a end src_)
        |= Parser.getOffset
        |= macroName generation lineNo
        |= optArg generation lineNo
        |= many (arg generation lineNo)
        |= Parser.getOffset
        |= Parser.getSource


macroName : Int -> Int -> Parser ( String, SourceMap )
macroName generation chunkOffset =
    Parser.succeed (\start str end -> ( str, { content = str, blockOffset = chunkOffset, length = end - start, offset = start, generation = generation } ))
        |= Parser.getOffset
        |= macroName2
        |= Parser.getOffset


fixMacro : Int -> Int -> Int -> ( String, SourceMap ) -> Maybe ( Expression, SourceMap ) -> List Expression -> Int -> String -> Expression
fixMacro generation start lineNo ( name, sm1 ) optArg_ args_ end src_ =
    let
        lineNumber =
            sm1.blockOffset

        sm =
            { content = src_, blockOffset = lineNumber, offset = start, length = end - start, generation = generation }
    in
    Macro name (Maybe.map Tuple.first optArg_) args_ sm


macroName2 : Parser String
macroName2 =
    Parser.variable
        { start = \c -> c == '\\'
        , inner = \c -> Char.isAlphaNum c || c == '*'
        , reserved = Set.fromList [ "\\begin", "\\end", "\\item", "\\bibitem" ]
        , expecting = RejectMacroReservedWord
        }
        |> Parser.map (String.dropLeft 1)


bareMacro : Int -> Int -> Parser Expression
bareMacro generation lineNo =
    Parser.succeed (\( name, sm ) -> Macro name Nothing [] sm)
        |= bareMacroName generation lineNo


bareMacroName : Int -> Int -> Parser ( String, SourceMap )
bareMacroName generation lineNo =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "\\" ExpectingBackslash)
        |= rawText generation lineNo [ ' ' ]
        |. Parser.symbol (Parser.Token " " ExpectingSpace)



-- OptArg


optArg : Int -> Int -> Parser (Maybe ( Expression, SourceMap ))
optArg generation lineNo =
    Parser.oneOf [ optArg__ generation lineNo |> Parser.map Just, Parser.succeed Nothing ]


optArg__ : Int -> Int -> Parser ( Expression, SourceMap )
optArg__ generation lineNo =
    Parser.inContext OptArgContext <|
        Parser.succeed identity
            |. Parser.symbol (Parser.Token "[" ExpectingLeftBracket)
            |= Parser.lazy (\_ -> chompExpression generation lineNo (expression generation lineNo))
            |. Parser.symbol (Parser.Token "]" ExpectingRightBracket)


optionalArg : Int -> Int -> Parser Expression
optionalArg generation chunkOffset =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "[" ExpectingLeftBracket)
        |= itemList (Parser.oneOf [ optArg2 generation chunkOffset, inlineMath generation chunkOffset ])
        |. Parser.symbol (Parser.Token "]" ExpectingRightBracket)
        |> Parser.map LXList


optArg2 : Int -> Int -> Parser Expression
optArg2 generation chunkOffset =
    optArg__ generation chunkOffset |> Parser.map Tuple.first


arg : Int -> Int -> Parser Expression
arg generation lineNo =
    Parser.inContext ArgContext <|
        Parser.succeed (\o1 e o2 -> fixArg o1 e o2)
            |= Parser.getOffset
            |. Parser.symbol (Parser.Token "{" ExpectingLeftBrace)
            |= Parser.oneOf
                [ inlineMath generation lineNo

                --, Parser.lazy (\_ -> Parser.oneOf [ Parser.backtrackable (bareMacro lineNo), macro lineNo ])
                , Parser.lazy (\_ -> macro generation lineNo)
                , text_ generation lineNo [ '}' ]
                ]
            |. Parser.symbol (Parser.Token "}" ExpectingRightBrace)
            |= Parser.getOffset


fixArg : Int -> Expression -> Int -> Expression
fixArg k1 e k2 =
    e



-- Environment


environment : Int -> Int -> Parser Expression
environment generation chunkOffset =
    Parser.succeed
        (\start expr end src ->
            Expression.setSourceMap { blockOffset = chunkOffset, length = end - start, offset = start, content = src, generation = generation } expr
        )
        |= Parser.getOffset
        |= (envName generation chunkOffset |> Parser.andThen (environmentOfType generation chunkOffset))
        |= Parser.getOffset
        |= Parser.getSource


{-| Capture the name of the environment in
a \\begin{ENV} ... \\end{ENV}
pair
-}
envName : Int -> Int -> Parser ( String, SourceMap )
envName generation chunkOffset =
    Parser.inContext EnvNameContext <|
        Parser.succeed
            (\start str end ->
                ( str
                , { content = "\\begin{" ++ str ++ "}", blockOffset = chunkOffset, offset = start, length = end - start, generation = generation }
                )
            )
            |= Parser.getOffset
            |. Parser.symbol (Parser.Token "\\begin{" ExpectingBegin)
            |= parseToSymbol ExpectingRightBrace "}"
            |= Parser.getOffset



{- DISPATCHER AND SUBPARSERS -}


environmentOfType : Int -> Int -> ( String, SourceMap ) -> Parser Expression
environmentOfType generation chunkOffset ( envType, sm ) =
    let
        theEndWord =
            "\\end{"
                ++ envType
                ++ "}"

        katex =
            [ "align", "matrix", "pmatrix", "bmatrix", "Bmatrix", "vmatrix", "Vmatrix" ]

        envKind =
            if List.member envType ([ "equation", "eqnarray", "verbatim", "colored", "CD", "mathmacro", "textmacro", "listing", "verse" ] ++ katex) then
                "passThrough"

            else
                envType
    in
    environmentParser generation chunkOffset envKind theEndWord envType


environmentParser : Int -> Int -> String -> String -> String -> Parser Expression
environmentParser generation chunkOffset envKind theEndWord envType =
    case Dict.get envKind environmentDict of
        Just p ->
            p generation chunkOffset theEndWord envType

        Nothing ->
            standardEnvironmentBody generation chunkOffset theEndWord envType


environmentDict : Dict.Dict String (Int -> Int -> String -> String -> Parser Expression)
environmentDict =
    Dict.fromList
        [ -- ( "enumerate", \endWoord envType -> itemEnvironmentBody endWoord envType )
          --, ( "itemize", \endWoord envType -> itemEnvironmentBody endWoord envType )
          --, ( "thebibliography", \endWoord envType -> biblioEnvironmentBody endWoord envType )
          --, ( "tabular", \endWoord envType -> tabularEnvironmentBody endWoord envType )
          ( "passThrough", \generation chunkOffset endWoord envType -> passThroughBody generation chunkOffset endWoord envType )
        ]


standardEnvironmentBody : Int -> Int -> String -> String -> Parser Expression
standardEnvironmentBody generation chunkOffset endWoord envType =
    -- Parser.succeed (fixExpr chunkOffset envType)
    Parser.succeed (\start oa body end src -> Environment envType oa body { content = src, blockOffset = chunkOffset, offset = start, length = end - start, generation = generation })
        |= Parser.getOffset
        |= many (optionalArg generation chunkOffset)
        |. Parser.spaces
        |= innerParseEnvironment generation chunkOffset
        |. Parser.spaces
        |. Parser.symbol (Parser.Token endWoord (ExpectingEndWord endWoord))
        |= Parser.getOffset
        |= Parser.getSource


innerParseEnvironment generation chunkOffset =
    many
        (Parser.oneOf
            [ macro generation chunkOffset
            , inlineMath generation chunkOffset
            , environmentText generation chunkOffset
            ]
        )
        |> Parser.map (\x -> LXList x)


environmentText generation chunkOffset =
    textNP generation chunkOffset [ '$', '\\' ] [ '$', '\\' ]


{-| The body of the environment is parsed as an LXString.
This parser is used for environments whose body is to be
passed to MathJax for processing and also for the verbatim
environment.
-}
passThroughBody : Int -> Int -> String -> String -> Parser Expression
passThroughBody generation chunkOffset endWoord envType =
    --  inContext "passThroughBody" <|
    Parser.succeed identity
        |= parseToSymbol ExpectingEndForPassThroughBody endWoord
        |. Parser.spaces
        |> Parser.map (passThroughEnv generation chunkOffset envType)


passThroughEnv : Int -> Int -> String -> String -> Expression
passThroughEnv generation chunkOffset envType source =
    let
        lines =
            source |> String.trim |> String.lines |> List.filter (\l -> String.length l > 0)

        data =
            passThroughData generation envType source lines

        sm =
            { blockOffset = chunkOffset, offset = data.offset, length = data.length, content = data.body, generation = generation }
    in
    Environment envType data.optArgs (Text data.body sm) sm


passThroughData generation envType source lines =
    let
        optArgs =
            -- TODO: copout
            runParser (itemList (optionalArg generation 0)) (List.head lines |> Maybe.withDefault "")

        envTypeLength =
            String.length envType + 8

        labelLength =
            if String.contains "\\label" source then
                lines |> List.head |> Maybe.map String.length |> Maybe.withDefault 0

            else
                0

        body : String
        body =
            if optArgs == [] then
                lines |> String.join "\n"

            else
                List.drop 1 lines |> String.join "\n"

        contentLength =
            String.length body
    in
    { offset = envTypeLength + labelLength + 1, length = contentLength - labelLength, body = body, optArgs = optArgs }


runParser : Parser (List Expression) -> String -> List Expression
runParser p str =
    let
        expr =
            Parser.run p str
    in
    case expr of
        Ok latexExpr ->
            latexExpr

        Err error ->
            [ LXError "error running Parser" GenericError Expression.dummySourceMap ]



-- SOURCE MAP


updateSourceMap : Expression -> Maybe Expression -> Expression
updateSourceMap e me =
    case me of
        Nothing ->
            e

        Just e1 ->
            let
                sm1 =
                    Expression.getSource e1

                sm =
                    Expression.getSource e

                sm2 =
                    { sm | offset = sm.offset + sm1.length }
            in
            Expression.setSourceMap sm2 e1



-- HELPERS
-- Loop


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s nextState =
    case nextState s of
        Loop s_ ->
            loop s_ nextState

        Done b ->
            b



-- Many


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


manyNonEmpty : Parser a -> Parser (List a)
manyNonEmpty p =
    p
        |> Parser.andThen (\x -> itemList_ [ x ] p)



-- Many2


many2 : (a -> Maybe a -> a) -> Parser a -> Parser (List a)
many2 f p =
    Parser.loop [] (manyHelp2 f p)


manyHelp2 : (a -> Maybe a -> a) -> Parser a -> List a -> Parser (Parser.Step (List a) (List a))
manyHelp2 f p vs =
    Parser.oneOf
        [ eof |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        , Parser.succeed (\v -> Parser.Loop (f v (List.head vs) :: vs))
            |= p
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        ]



-- ItemList


nonEmptyItemList : Parser a -> Parser (List a)
nonEmptyItemList itemParser =
    itemParser
        |> Parser.andThen (\x -> itemList_ [ x ] itemParser)


itemList : Parser a -> Parser (List a)
itemList itemParser =
    itemList_ [] itemParser


itemList_ : List a -> Parser a -> Parser (List a)
itemList_ initialList itemParser =
    Parser.loop initialList (itemListHelper itemParser)


itemListHelper : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
itemListHelper itemParser revItems =
    Parser.oneOf
        [ Parser.succeed (\item_ -> Parser.Loop (item_ :: revItems))
            |= itemParser
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revItems))
        ]


eof : Parser ()
eof =
    Parser.end EndOfInput


{-| chomp to end of the marker and return the
chomped string minus the marker.
-}
parseToSymbol : Problem -> String -> Parser String
parseToSymbol problem marker =
    (Parser.getChompedString <|
        Parser.succeed identity
            |= Parser.chompUntilEndOr marker
            |. Parser.symbol (Parser.Token marker problem)
    )
        |> Parser.map (String.dropRight (String.length marker))
