module Parser.Parser exposing (..)

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


## Helpers

@docs renderToStingList, getStringAtWithDefault, renderArg


## Uses of Parser.Parser

  - parseLoop in Parser.Document
  - getErrors in Main

-}

{-

   ( Parser
   , parseLoop, expression, expressionList, parseExpression, macro
   , getStringAtWithDefault, renderArg
   , item, optionalArg, parse, renderToStringList
   )

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
import List.Extra
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Expression as Expression exposing (Context(..), Expression(..), Problem(..), SourceMap)
import Parser.Problem exposing (RecoveryData)
import Parser.TextCursor as TextCursor exposing (TextCursor)
import Parser.ToolAdvanced as ParserTool
import Set


{-| -}
type alias Parser a =
    Parser.Parser Context Problem a



-- PARSELOOP


{-| parseLoop takes as input an integer representing a "chunkOffset" and
a string of source text, the "chunk." It returns a TextCursor, which
is a data structure which includes the parsed source text.
-}
parseLoop : Int -> Int -> String -> TextCursor
parseLoop generation initialLineNumber str =
    ParserTool.loop (TextCursor.init generation initialLineNumber str) nextCursor


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
nextCursor : TextCursor -> ParserTool.Step TextCursor TextCursor
nextCursor tc =
    if tc.text == "" || tc.count > 10 then
        -- TODO: that usage of count needs to be removed after bug is fixed
        ParserTool.Done { tc | parsed = List.reverse tc.parsed }

    else
        case Parser.run (expression tc.generation tc.blockIndex) tc.text of
            Ok expr ->
                let
                    sourceMap =
                        Expression.getSource expr
                in
                ParserTool.Loop
                    { tc
                        | count = tc.count + 1
                        , text = String.dropLeft sourceMap.length tc.text
                        , block = tc.block ++ String.left sourceMap.length tc.text
                        , parsed = newExpr tc expr :: tc.parsed
                        , offset = tc.offset + sourceMap.length
                    }

            Err e ->
                ParserTool.Loop (handleError tc e)


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
            mFirstError |> Maybe.map .problem |> Maybe.withDefault UnHandledError

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
    ParserTool.many (expression generation lineNumber)


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
        [ comment generation lineNumber
        , newcommand generation lineNumber
        , macro generation lineNumber
        , environment generation lineNumber
        , displayMath generation lineNumber
        , inlineMath generation lineNumber
        , text_ generation lineNumber [ '$', '\\' ]
        ]


{-| Transform a string into a list of LatexExpressions

    parse "Pythagoras: $a^2 + b^2 = c^2$"
    --> [LXString ("Pythagoras: "),InlineMath ("a^2 + b^2 = c^2")]

-}
parse : String -> List Expression
parse str =
    case Parser.run (expressionList 0 0) str of
        Ok list ->
            list

        Err _ ->
            -- TODO: vvv very bad code.  Fix this! vvv
            [ LXError "Error parsing expression list" UnHandledError Expression.dummySourceMap ]



-- TEXT


textNP : Int -> Int -> List Char -> List Char -> Parser Expression
textNP generation lineNumber prefixChars stopChars =
    let
        makeSM =
            Expression.makeSourceMap generation lineNumber
    in
    ParserTool.textPS (\c -> not (List.member c prefixChars)) stopChars
        |> Parser.map (\data -> Text data.content (makeSM data.start data.finish data.content))


text_ : Int -> Int -> List Char -> Parser Expression
text_ generation lineNumber stopChars =
    Parser.map (\( t, s ) -> Text t s) (rawText generation lineNumber stopChars)


rawText : Int -> Int -> List Char -> Parser ( String, SourceMap )
rawText gneration lineNumber stopChars =
    getChompedString gneration lineNumber <|
        Parser.succeed ()
            |. Parser.chompWhile (\c -> not (List.member c stopChars))


wordX : Problem -> (Char -> Bool) -> Parser String
wordX problem inWord =
    Parser.succeed String.slice
        |= Parser.getOffset
        |. Parser.chompIf inWord problem
        |. Parser.chompWhile inWord
        |. Parser.spaces
        |= Parser.getOffset
        |= Parser.getSource


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



-- Comment


comment : Int -> Int -> Parser Expression
comment generation lineNumber =
    -- Parser.inContext Comment <|
    Parser.succeed (\( s, t ) -> Comment s { t | length = String.length s + 1 })
        |. Parser.symbol (Parser.Token "%" ExpectingLeadingPercentSign)
        |= getChompedString generation lineNumber (Parser.chompUntilEndOr "\n")
        -- ^^ TODO: Hmm ...
        |. Parser.spaces



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



-- Newcommand
--     { blockOffset = 0, length = 0, offset = 0, content = "", generation = 0 }


newcommand : Int -> Int -> Parser Expression
newcommand generation lineNumber =
    Parser.succeed (\first_ name nargs arg_ last_ src -> NewCommand name nargs arg_ { blockOffset = lineNumber, generation = generation, length = last_ - first_, offset = first_, content = src })
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token "\\newcommand{" ExpectingLeftBrace)
        |= (macroName 0 0 |> Parser.map Tuple.first)
        |. Parser.symbol (Parser.Token "}" ExpectingRightBrace)
        |= numberOfArgs
        |= argForNewCommand
        |. Parser.spaces
        |= Parser.getOffset
        |= Parser.getSource


{-|

    import LXParser

    LXParser.run numberOfArgs "[3]"
    --> Ok 3

-}
numberOfArgs : Parser Int
numberOfArgs =
    ParserTool.many numberOfArgs_
        |> Parser.map List.head
        |> Parser.map (Maybe.withDefault 0)


numberOfArgs_ : Parser Int
numberOfArgs_ =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "[" ExpectingLeftBracket)
        |= Parser.int ExpectingInt InvalidInt
        |. Parser.symbol (Parser.Token "]" ExpectingRightBracket)



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
        |= ParserTool.many (arg generation lineNo)
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


optionalArg : Int -> Int -> Parser Expression
optionalArg generation chunkOffset =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "[" ExpectingLeftBracket)
        |= ParserTool.many (Parser.oneOf [ optArg2 generation chunkOffset, inlineMath generation chunkOffset ])
        |. Parser.symbol (Parser.Token "]" ExpectingRightBracket)
        |> Parser.map LXList


optArg2 : Int -> Int -> Parser Expression
optArg2 generation chunkOffset =
    optArg__ generation chunkOffset |> Parser.map Tuple.first


optArg__ : Int -> Int -> Parser ( Expression, SourceMap )
optArg__ generation lineNo =
    Parser.inContext OptArgContext <|
        Parser.succeed identity
            |. Parser.symbol (Parser.Token "[" ExpectingLeftBracket)
            |= Parser.lazy (\_ -> chompExpression generation lineNo (expression generation lineNo))
            |. Parser.symbol (Parser.Token "]" ExpectingRightBracket)


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


argForNewCommand : Parser Expression
argForNewCommand =
    (Parser.succeed identity
        |. Parser.symbol (Parser.Token "{" ExpectingLeftBrace)
        |= ParserTool.many (Parser.oneOf [ macroArgWords, inlineMath 0 0, Parser.lazy (\_ -> macro 0 0) ])
        |. Parser.symbol (Parser.Token "}" ExpectingRightBrace)
    )
        |> Parser.map LXList


macroArgWords : Parser Expression
macroArgWords =
    ParserTool.manyNonEmpty (wordX ExpectingValidMacroArgWord inMacroArg)
        |> Parser.map (String.join " ")
        |> Parser.map (\s -> Text s Expression.dummySourceMap)


inMacroArg : Char -> Bool
inMacroArg c =
    not (c == '\\' || c == '$' || c == '}' || c == ' ' || c == '\n')


argBracket : Int -> Int -> Parser Expression
argBracket generation lineNo =
    Parser.inContext ArgContext <|
        Parser.succeed (\o1 e o2 -> fixArg o1 e o2)
            |= Parser.getOffset
            |. Parser.symbol (Parser.Token "[" ExpectingLeftBracket)
            |= Parser.oneOf
                [ inlineMath generation lineNo

                --, Parser.lazy (\_ -> Parser.oneOf [ Parser.backtrackable (bareMacro lineNo), macro lineNo ])
                , Parser.lazy (\_ -> macro generation lineNo)
                , text_ generation lineNo [ ']' ]
                ]
            |. Parser.symbol (Parser.Token "]" ExpectingRightBracket)
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
        |= (envName generation chunkOffset |> Parser.andThen (environment_ generation chunkOffset))
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


environment_ : Int -> Int -> ( String, SourceMap ) -> Parser Expression
environment_ generation chunkOffset ( envType, sm ) =
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
    case Dict.get envKind environmentDict of
        Just p ->
            p generation chunkOffset theEndWord envType

        Nothing ->
            Parser.succeed (\start oa body end src -> Environment envType oa body { content = src, blockOffset = chunkOffset, offset = start, length = end - start, generation = generation })
                |= Parser.getOffset
                |= ParserTool.many (argBracket generation chunkOffset)
                |. Parser.spaces
                |= innerParseEnvironment generation chunkOffset
                |. Parser.spaces
                |. Parser.symbol (Parser.Token theEndWord (ExpectingEndWord theEndWord))
                |= Parser.getOffset
                |= Parser.getSource


environmentDict : Dict.Dict String (Int -> Int -> String -> String -> Parser Expression)
environmentDict =
    Dict.fromList
        [ ( "enumerate", \generation chunkOffset endWoord envType -> itemEnvironmentBody generation chunkOffset endWoord envType )
        , ( "itemize", \endWoord envType -> itemEnvironmentBody endWoord envType )

        --, ( "thebibliography", \endWoord envType -> biblioEnvironmentBody endWoord envType )
        , ( "tabular", \endWoord envType -> tabularEnvironmentBody endWoord envType )
        , ( "passThrough", \generation chunkOffset endWoord envType -> passThroughBody generation chunkOffset endWoord envType )
        ]


innerParseEnvironment generation chunkOffset =
    ParserTool.many
        (Parser.oneOf
            [ environment generation chunkOffset
            , macro generation chunkOffset
            , displayMath generation chunkOffset
            , inlineMath generation chunkOffset
            , environmentText generation chunkOffset
            ]
        )
        |> Parser.map (\x -> LXList x)


environmentText : Int -> Int -> Parser Expression
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
            runParser (ParserTool.many (optionalArg generation 0)) (List.head lines |> Maybe.withDefault "")

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
            [ LXError "error running Parser" UnHandledError Expression.dummySourceMap ]



-- Itemize and enumerate


itemEnvironmentBody : Int -> Int -> String -> String -> Parser Expression
itemEnvironmentBody generation chunkOffset endWoord envType =
    ---  inContext "itemEnvironmentBody" <|
    Parser.succeed (\start expr finish src -> Environment envType [] (LXList expr) (Expression.makeSourceMap generation chunkOffset start finish src))
        |= Parser.getOffset
        |. Parser.spaces
        |= ParserTool.many (Parser.oneOf [ item generation chunkOffset, Parser.lazy (\_ -> environment generation chunkOffset) ])
        |. Parser.spaces
        |. Parser.symbol (Parser.Token endWoord (ExpectingEndWordInItemList endWoord))
        |. Parser.spaces
        |= Parser.getOffset
        |= Parser.getSource


item : Int -> Int -> Parser Expression
item generation lineNumber =
    ---  inContext "item" <|
    Parser.succeed (\start e finish src -> Item 1 (LXList e) (Expression.makeSourceMap generation lineNumber start finish src))
        |= Parser.getOffset
        |. Parser.spaces
        |. Parser.symbol (Parser.Token "\\item" ExpectingEscapedItem)
        |. Parser.symbol (Parser.Token " " ExpectingSpaceAfterItem)
        |. Parser.spaces
        |= ParserTool.many (Parser.oneOf [ textNP generation lineNumber [ '$', '\\' ] [ '$', '\\' ], inlineMath generation lineNumber, macro generation lineNumber ])
        |. Parser.spaces
        |= Parser.getOffset
        |= Parser.getSource



-- TABLES
{- TABLES -}


tabularEnvironmentBody : Int -> Int -> String -> String -> Parser Expression
tabularEnvironmentBody generation lineNumber endWoord envType =
    -- inContext "tabularEnvironmentBody" <|
    Parser.succeed (\start args body finish src -> Environment envType args body (Expression.makeSourceMap generation lineNumber start finish src))
        |= Parser.getOffset
        |. Parser.spaces
        |= ParserTool.many (arg generation lineNumber)
        |= tableBody generation lineNumber
        |. Parser.spaces
        |. Parser.symbol (Parser.Token endWoord (ExpectingEndWord endWoord))
        |. Parser.spaces
        |= Parser.getOffset
        |= Parser.getSource


tableBody : Int -> Int -> Parser Expression
tableBody generation lineNumber =
    --  inContext "tableBody" <|
    Parser.succeed (\star row finish src -> LXList row)
        --|. repeat zeroOrMore arg
        |= Parser.getOffset
        |. Parser.spaces
        -- |= ParserTool.manyNonEmpty (tableRow generation lineNumber)
        |= ParserTool.manySeparatedBy endOfTableLine (tableRow generation lineNumber)
        |= Parser.getOffset
        |= Parser.getSource


endOfTableLine : Parser ()
endOfTableLine =
    ParserTool.first (Parser.symbol (Parser.Token "\\\\" (Expression.ExpectingChar '\\'))) Parser.spaces


tableRow : Int -> Int -> Parser Expression
tableRow generation lineNumber =
    --- inContext "tableRow" <|
    Parser.succeed (\start ll finish src -> LXList ll)
        |= Parser.getOffset
        |. Parser.spaces
        |= ParserTool.manySeparatedBy ampersand (tableCell generation lineNumber)
        |. Parser.spaces
        -- |. Parser.oneOf [ Parser.symbol (Parser.Token "\n" (Expression.ExpectingChar '\n')), Parser.symbol (Parser.Token "\\\\\n" (Expression.ExpectingChar '\\')) ]
        |= Parser.getOffset
        |= Parser.getSource


ampersand : Parser ()
ampersand =
    ParserTool.first (Parser.symbol (Parser.Token "&" (Expression.ExpectingChar '&'))) Parser.spaces



-- ###


tableCell : Int -> Int -> Parser Expression
tableCell generation lineNumber =
    -- inContext "tableCell" <|
    Parser.succeed (\start e finish src -> e)
        |= Parser.getOffset
        |= (innerTableCell generation lineNumber |> Parser.map LXList)
        |= Parser.getOffset
        |= Parser.getSource


innerTableCell generation lineNumber =
    ParserTool.many
        (Parser.oneOf
            -- TODO: the clause below needs to handle lists of what is listed below
            [ tableCellWords generation lineNumber
            , displayMath generation lineNumber
            , inlineMath generation lineNumber

            -- , macro generation lineNumber
            ]
        )


tableCellWords : Int -> Int -> Parser Expression
tableCellWords generation lineNumber =
    -- ParserTool.manyNonEmpty (word ExpectingValidTableCell inTableCellWord)
    ParserTool.textPS beginTableCellWord [ '\n', '$', '&', '\\' ]
        |> Parser.map (\data -> Text data.content (Expression.makeSourceMap generation lineNumber data.start data.finish data.content))


beginTableCellWord : Char -> Bool
beginTableCellWord c =
    not (c == '\\' || c == '$' || c == '&' || c == '\n')


tableCellHelp : Int -> Int -> List Expression -> Parser (List Expression)
tableCellHelp generation lineNumber revCells =
    --  inContext "tableCellHelp" <|
    Parser.oneOf
        [ nextCell generation lineNumber
            |> Parser.andThen (\c -> tableCellHelp generation lineNumber (c :: revCells))
        , Parser.succeed (List.reverse revCells)
        ]


nextCell : Int -> Int -> Parser Expression
nextCell generation lineNumber =
    --  inContext "nextCell" <|
    -- (delayedCommit spaces <|
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "&" Expression.UnHandledError)
        |. Parser.spaces
        |= tableCell generation lineNumber



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


{-| -}
renderToStringList : List Expression -> List String
renderToStringList exprs =
    List.map Expression.toString exprs


{-| -}
getStringAtWithDefault : Int -> String -> List String -> String
getStringAtWithDefault k default strings =
    List.Extra.getAt k strings |> Maybe.withDefault default


{-| render 0-th arg to string
-}
renderArg : List Expression -> String
renderArg expressions =
    List.Extra.getAt 0 (List.map Expression.toString expressions) |> Maybe.withDefault "ARG"
