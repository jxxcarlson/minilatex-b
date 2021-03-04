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

@docs parseLoop, getErrors


## Uses of Parser.Parser

  - parseLoop in Parser.Document
  - getErrors in Main

-}

--( parseLoop, getErrors
--, envName, environment, expression, expressionList, macroName, many, nonEmptyItemList, standardEnvironmentBody, textNP
--)

import Dict
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Expression as Expression exposing (Expression(..), Problem(..), SourceMap)
import Parser.TextCursor as TextCursor exposing (TextCursor)
import Set
import Utility


type alias Parser a =
    Parser.Parser Context Problem a


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
parseLoop : Int -> String -> TextCursor
parseLoop initialLineNumber str =
    loop (TextCursor.init initialLineNumber str) nextCursor


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
        Done { tc | parsed = List.reverse tc.parsed }

    else
        case Parser.run (expression tc.chunkNumber) tc.text of
            Ok expr ->
                let
                    sourceMap =
                        Expression.getSource expr
                in
                Loop
                    { tc
                        | count = tc.count + 1
                        , text = String.dropLeft sourceMap.length tc.text
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
            getRecoveryData_ tc_ problem

        lxError =
            LXError errorText problem { content = errorText, chunkOffset = tc_.chunkNumber, length = errorColumn, offset = tc_.offset + errorColumn }
    in
    { text = makeNewText tc_ errorColumn mRecoveryData
    , chunkNumber = tc_.chunkNumber
    , parsed = newParsed tc_ lxError mRecoveryData
    , stack = newStack tc_ errorText mRecoveryData
    , offset = newOffset tc_ errorColumn mRecoveryData
    , count = 0
    }


type alias RecoveryData =
    { problem : Problem
    , deltaOffset : Int
    , textTruncation : Int
    , parseSubstitute : Expression
    }


getRecoveryData_ : TextCursor -> Problem -> Maybe RecoveryData
getRecoveryData_ tc_ problem =
    let
        oldSourceMap =
            Expression.dummySourceMap

        newSourceMap =
            { oldSourceMap | chunkOffset = tc_.chunkNumber }
    in
    getRecoveryData problem
        |> Maybe.map (\r -> { r | parseSubstitute = Expression.setSourceMap newSourceMap r.parseSubstitute })


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


getRecoveryData : Problem -> Maybe RecoveryData
getRecoveryData problem =
    List.filter (\r -> Expression.equivalentProblem r.problem problem) recoveryData |> List.head


recoveryData : List RecoveryData
recoveryData =
    [ problemWithInlineMath, problemWithDisplayMath, problemWithEnvironment, problemWithMacro ]


problemWithMacro : RecoveryData
problemWithMacro =
    { problem = ExpectingRightBrace
    , deltaOffset = 0
    , textTruncation = 1
    , parseSubstitute =
        LXList
            [ Macro "red"
                Nothing
                [ Text "!! missing right brace in \\"
                    Expression.dummySourceMap
                ]
                Expression.dummySourceMap
            ]
    }


problemWithEnvironment : RecoveryData
problemWithEnvironment =
    { problem = ExpectingEndWord "dummy"
    , deltaOffset = 3
    , textTruncation = 2
    , parseSubstitute =
        LXList
            [ Macro "red"
                Nothing
                [ DisplayMath "!! unmatched \\begin .. \\end: " Expression.dummySourceMap
                ]
                Expression.dummySourceMap
            ]
    }


problemWithDisplayMath : RecoveryData
problemWithDisplayMath =
    { problem = ExpectingTrailingDoubleDollarSign
    , deltaOffset = 2
    , textTruncation = 2 -- corresponds to "$$"
    , parseSubstitute =
        LXList
            [ Macro "red"
                Nothing
                [ Text ("!! unmatched $$ in" ++ String.fromChar '\u{00A0}') Expression.dummySourceMap
                ]
                Expression.dummySourceMap
            ]
    }


problemWithInlineMath : RecoveryData
problemWithInlineMath =
    { problem = ExpectingTrailingDollarSign
    , deltaOffset = 1
    , textTruncation = 1
    , parseSubstitute =
        LXList
            [ Macro "red"
                Nothing
                [ Text ("!! unmatched $ in" ++ String.fromChar '\u{00A0}') Expression.dummySourceMap
                ]
                Expression.dummySourceMap
            ]
    }



-- EXPRESSION


{-|

> run expressionList "foo bar $a^2$ baz"
> Ok [Text ("foo bar "),InlineMath "a^2",Text "baz"]

-}
expressionList : Int -> Parser (List Expression)
expressionList lineNumber =
    many (expression lineNumber)


expression : Int -> Parser.Parser Context Problem Expression
expression lineNumber =
    Parser.oneOf
        [ macro lineNumber
        , environment lineNumber
        , displayMath lineNumber
        , inlineMath lineNumber
        , text lineNumber

        --, alwaysP -- TODO: examine the wisdom of adding this
        ]


alwaysP =
    Parser.succeed (LXInstruction Expression.INoOp { chunkOffset = 0, length = 1, offset = 0, content = "" })



-- ERRORS


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


textNP : Int -> List Char -> List Char -> Parser Expression
textNP lineNumber prefixChars stopChars =
    let
        sm : Int -> Int -> String -> Expression
        sm start end src =
            let
                content =
                    String.slice start end src

                sm_ =
                    { content = content, length = end - start, chunkOffset = lineNumber, offset = start }
            in
            Text content sm_
    in
    Parser.succeed sm
        |= Parser.getOffset
        |. Parser.chompIf (\c -> not (List.member c prefixChars)) (ExpectingPrefixes prefixChars)
        |. Parser.chompWhile (\c -> not (List.member c stopChars))
        |= Parser.getOffset
        |= Parser.getSource


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
    Parser.inContext InlineMathContext <|
        Parser.succeed (\( s, t ) -> InlineMath s { t | length = t.length + 1 })
            |. Parser.symbol (Parser.Token "$" ExpectingLeadingDollarSign)
            |= getChompedString lineNumber (Parser.chompUntil (Parser.Token "$" ExpectingTrailingDollarSign))
            |. Parser.symbol (Parser.Token "$" ExpectingTrailingDollarSign)
            |. Parser.spaces


displayMath : Int -> Parser Expression
displayMath lineNumber =
    Parser.inContext DisplayMathContext <|
        Parser.succeed (\( s, t ) -> DisplayMath s { t | length = t.length + 2 })
            |. Parser.symbol (Parser.Token "$$" ExpectingLeadingDoubleDollarSign)
            |= getChompedString lineNumber (Parser.chompUntil (Parser.Token "$$" ExpectingTrailingDoubleDollarSign))
            |. Parser.symbol (Parser.Token "$$" ExpectingTrailingDoubleDollarSign)
            |. Parser.spaces



-- MACRO


{-|

    Use this to parse a string and return information about its location in the source

-}
getChompedString : Int -> Parser a -> Parser ( String, SourceMap )
getChompedString lineNumber parser =
    let
        sm first_ last_ source_ =
            let
                src =
                    String.slice first_ last_ source_
            in
            ( src, { content = src, chunkOffset = lineNumber, length = last_, offset = 0 } )
    in
    Parser.succeed sm
        |= Parser.getOffset
        |. parser
        |= Parser.getOffset
        |= Parser.getSource


macro : Int -> Parser Expression
macro lineNo =
    Parser.succeed (\start n o a end src_ -> fixMacro start lineNo n o a end src_)
        |= Parser.getOffset
        |= macroName lineNo
        |= optArg lineNo
        |= many (arg lineNo)
        |= Parser.getOffset
        |= Parser.getSource


bareMacro : Int -> Parser Expression
bareMacro lineNo =
    Parser.succeed (\( name, sm ) -> Macro name Nothing [] sm)
        |= bareMacroName lineNo


fixMacro : Int -> Int -> ( String, SourceMap ) -> Maybe ( String, SourceMap ) -> List Expression -> Int -> String -> Expression
fixMacro start lineNo ( name, sm1 ) optArg_ args_ end src_ =
    let
        lineNumber =
            sm1.chunkOffset

        sm =
            { content = src_, chunkOffset = lineNumber, offset = start, length = end - start }
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


macroName : Int -> Parser ( String, SourceMap )
macroName chunkOffset =
    Parser.succeed (\start str end -> ( str, { content = str, chunkOffset = chunkOffset, length = end - start, offset = start } ))
        |= Parser.getOffset
        |= macroName2
        |= Parser.getOffset


bareMacroName : Int -> Parser ( String, SourceMap )
bareMacroName lineNo =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "\\" ExpectingBackslash)
        |= rawText lineNo [ ' ' ]
        |. Parser.symbol (Parser.Token " " ExpectingSpace)


optArg : Int -> Parser (Maybe ( String, SourceMap ))
optArg lineNo =
    Parser.oneOf [ optArg__ lineNo |> Parser.map Just, Parser.succeed Nothing ]


optArg__ : Int -> Parser ( String, SourceMap )
optArg__ lineNo =
    Parser.inContext OptArgContext <|
        Parser.succeed identity
            |. Parser.symbol (Parser.Token "[" ExpectingLeftBracket)
            |= rawText lineNo [ ']' ]
            |. Parser.symbol (Parser.Token "]" ExpectingRightBracket)


arg : Int -> Parser Expression
arg lineNo =
    Parser.inContext ArgContext <|
        Parser.succeed (\o1 e o2 -> fixArg o1 e o2)
            |= Parser.getOffset
            |. Parser.symbol (Parser.Token "{" ExpectingLeftBrace)
            |= Parser.oneOf
                [ inlineMath lineNo

                --, Parser.lazy (\_ -> Parser.oneOf [ Parser.backtrackable (bareMacro lineNo), macro lineNo ])
                , Parser.lazy (\_ -> macro lineNo)
                , text_ lineNo [ '}' ]
                ]
            |. Parser.symbol (Parser.Token "}" ExpectingRightBrace)
            |= Parser.getOffset


fixArg : Int -> Expression -> Int -> Expression
fixArg k1 e k2 =
    e



-- ENVIRONMENT
--environment : Int -> Parser Expression


environment : Int -> Parser Expression
environment chunkOffset =
    Parser.succeed
        (\start expr end src ->
            Expression.setSourceMap { chunkOffset = chunkOffset, length = end - start, offset = start, content = src } expr
        )
        |= Parser.getOffset
        |= (envName chunkOffset |> Parser.andThen (environmentOfType chunkOffset))
        |= Parser.getOffset
        |= Parser.getSource


{-| Capture the name of the environment in
a \\begin{ENV} ... \\end{ENV}
pair
-}
envName : Int -> Parser ( String, SourceMap )
envName chunkOffset =
    Parser.inContext EnvNameContext <|
        Parser.succeed
            (\start str end ->
                ( str
                , { content = "\\begin{" ++ str ++ "}", chunkOffset = chunkOffset, offset = start, length = end - start }
                )
            )
            |= Parser.getOffset
            |. Parser.symbol (Parser.Token "\\begin{" ExpectingBegin)
            |= parseToSymbol ExpectingRightBrace "}"
            |= Parser.getOffset



{- DISPATCHER AND SUBPARSERS -}


environmentOfType : Int -> ( String, SourceMap ) -> Parser Expression
environmentOfType chunkOffset ( envType, sm ) =
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
    environmentParser chunkOffset envKind theEndWord envType


environmentParser : Int -> String -> String -> String -> Parser Expression
environmentParser chunkOffset envKind theEndWord envType =
    case Dict.get envKind environmentDict of
        Just p ->
            p theEndWord envType

        Nothing ->
            standardEnvironmentBody chunkOffset theEndWord envType


environmentDict : Dict.Dict String (String -> String -> Parser Expression)
environmentDict =
    Dict.fromList
        []


xxx =
    """
\\begin{theorem}
Many primes!
\\end{theorem}
"""


standardEnvironmentBody : Int -> String -> String -> Parser Expression
standardEnvironmentBody chunkOffset endWoord envType =
    -- Parser.succeed (fixExpr chunkOffset envType)
    Parser.succeed (\start oa body end src -> Environment envType oa body { content = src, chunkOffset = chunkOffset, offset = start, length = end - start })
        |= Parser.getOffset
        |= many (optionalArg chunkOffset)
        |. Parser.spaces
        |= innerParseEnvironment chunkOffset
        |. Parser.spaces
        |. Parser.symbol (Parser.Token endWoord (ExpectingEndWord endWoord))
        |= Parser.getOffset
        |= Parser.getSource


innerParseEnvironment1 chunkOffset =
    many2 updateSourceMap
        (Parser.oneOf
            [ macro chunkOffset
            , inlineMath chunkOffset
            , environmentText chunkOffset
            ]
        )
        |> Parser.map (\x -> LXList x)


innerParseEnvironment chunkOffset =
    many
        (Parser.oneOf
            [ macro chunkOffset
            , inlineMath chunkOffset
            , environmentText chunkOffset
            ]
        )
        |> Parser.map (\x -> LXList x)


fixExpr chunkOffset envType start oa body end src =
    let
        sm =
            Expression.getSource body
    in
    Environment envType oa body { content = src, chunkOffset = chunkOffset, offset = start, length = sm.length }


environmentText chunkOffset =
    textNP chunkOffset [ '$', '\\' ] [ '$', '\\' ]


{-| The body of the environment is parsed as an LXString.
This parser is used for environments whose body is to be
passed to MathJax for processing and also for the verbatim
environment.
-}



-- TODO


passThroughBody : String -> String -> Parser Expression
passThroughBody endWoord envType =
    --  inContext "passThroughBody" <|
    Parser.succeed identity
        |= parseToSymbol ExpectingEndForPassThroughBody endWoord
        |. Parser.spaces
        |> Parser.map (passThroughEnv envType)


passThroughEnv : String -> String -> Expression
passThroughEnv envType source =
    let
        lines =
            source |> String.trim |> String.lines |> List.filter (\l -> String.length l > 0)

        optArgs_ =
            -- TODO: copout
            runParser (itemList (optionalArg 0)) (List.head lines |> Maybe.withDefault "")

        body : String
        body =
            if optArgs_ == [] then
                lines |> String.join "\n"

            else
                List.drop 1 lines |> String.join "\n"
    in
    -- TODO: remove cop out!
    Environment envType optArgs_ (Text body Expression.dummySourceMap) Expression.dummySourceMap


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


optionalArg : Int -> Parser Expression
optionalArg chunkOffset =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "[" ExpectingLeftBracket)
        |= itemList (Parser.oneOf [ optArg2 chunkOffset, inlineMath chunkOffset ])
        |. Parser.symbol (Parser.Token "]" ExpectingRightBracket)
        |> Parser.map LXList


optArg2 : Int -> Parser Expression
optArg2 chunkOffset =
    optArg__ chunkOffset |> Parser.map (\( s, sm ) -> Text s sm)


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


inOptionArgWord : Char -> Bool
inOptionArgWord c =
    not (c == '\\' || c == '$' || c == ']' || c == ' ' || c == '\n')


manyNonEmpty : Parser a -> Parser (List a)
manyNonEmpty p =
    p
        |> Parser.andThen (\x -> itemList_ [ x ] p)


nonEmptyItemList : Parser a -> Parser (List a)
nonEmptyItemList itemParser =
    itemParser
        |> Parser.andThen (\x -> itemList_ [ x ] itemParser)


itemList : Parser a -> Parser (List a)
itemList itemParser =
    itemList_ [] itemParser



-- manyHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))


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



-- WORD


word : Int -> Parser ( String, SourceMap )
word lineNumber =
    Parser.inContext WordContext <|
        Parser.succeed identity
            |= getChompedString lineNumber (Parser.chompUntil (Parser.Token " " ExpectingEndOfWordSpace))



-- LOOP


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
