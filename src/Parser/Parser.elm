module Parser.Parser exposing (parseLoop, expressionList)


import Parser.Advanced as Parser exposing ((|.), (|=))

import Parser.Expression as Expression exposing(Expression(..), Problem(..), SourceMap)

import Parser.TextCursor as TextCursor exposing(TextCursor)

type alias Parser a = Parser.Parser Context Problem a

type Context = Foo | Bar

parseLoop : String -> TextCursor
parseLoop str =
    loop (TextCursor.init str) nextRound

nextRound : TextCursor -> Step TextCursor TextCursor
nextRound tc =
    if tc.text == "" then Done {tc | parsed = List.reverse tc.parsed}
    else
        case Parser.run expression tc.text of
            Ok expr ->
                let
                   sourceMap = Expression.getSource expr
                   newText = String.dropLeft sourceMap.length tc.text
                   newExpr = Expression.incrementOffset tc.offset expr
                in
                Loop { tc| text = newText, parsed = newExpr::tc.parsed, offset = tc.offset + sourceMap.length}
            Err e ->
                Loop (handleError tc e)


handleError tc_ e =
    let
       mFirstError = e |> List.head
       problem = Maybe.map .problem mFirstError |> Maybe.withDefault GenericError
       errorColumn =
          mFirstError |> Maybe.map .col |> Maybe.withDefault 0
       errorText = String.left errorColumn tc_.text
       errorColumn2 = case Parser.run word errorText of
            Ok (_,t) -> t.length
            Err err -> 4
       errorText2 = String.left errorColumn2 errorText
       newText = String.dropLeft errorColumn2 tc_.text
    in
    { text = newText
     , parsed = (LXError errorText2 problem {length = errorColumn2, offset = tc_.offset + errorColumn2}) :: tc_.parsed
     , stack = errorText :: tc_.stack
     , offset = tc_.offset + errorColumn}


{-|

    Use this to parse a string and return information about its location in the source

-}
getChompedString : Parser a -> Parser (String, SourceMap)
getChompedString parser =
  Parser.succeed (\first_ last_ source_ -> (String.slice first_ last_ source_, {length = last_, offset = 0}))
    |= Parser.getOffset
    |. parser
    |= Parser.getOffset
    |= Parser.getSource

{-|

> run expressionList "foo bar $a^2$ baz"
Ok [Text ("foo bar "),InlineMath "a^2",Text "baz"]

-}
expressionList : Parser (List Expression)
expressionList = many expression

expression : Parser.Parser Context Problem Expression
expression = Parser.oneOf [displayMath, inlineMath, text]


-- TEXT

text : Parser Expression
text = text_ ['$', '\\']

text_ : List Char -> Parser Expression
text_ stopChars =
  Parser.map (\(t,s) -> Text t s)  (rawText stopChars)


rawText : List Char -> Parser (String, SourceMap)
rawText stopChars =
  getChompedString <|
        Parser.succeed ()
          |. Parser.chompWhile (\c -> not (List.member c stopChars))


-- MATH

inlineMath : Parser Expression
inlineMath =
    Parser.succeed (\(s, t) -> InlineMath s  {t | length = t.length + 1})
      |. Parser.symbol (Parser.Token "$" ExpectingLeadingDollarSign)
      |= getChompedString (Parser.chompUntil (Parser.Token "$" ExpectingTrailingDollarSign1))
      |. Parser.symbol (Parser.Token "$" ExpectingTrailingDollarSign2)
      |. Parser.spaces

displayMath : Parser Expression
displayMath =
    Parser.succeed (\(s,t) -> DisplayMath s  {t | length = t.length + 2})
      |. Parser.symbol (Parser.Token "$$" ExpectingLeadingDoubleDollarSign)
      |= getChompedString (Parser.chompUntil (Parser.Token "$$" ExpectingLTrailingDoubleDollarSign1))
      |. Parser.symbol (Parser.Token "$$" ExpectingLTrailingDoubleDollarSign2)
      |. Parser.spaces


-- MACRO

--macro : Parser Expression
--macro =
--    Parser.succeed Macro

--macroName : Parser String
--macroName =
--    Parser.succeed identity
--      |. Parser.symbol (Parser.Token "\\" ExpectingLeadingBackslashForMacro)
--      |= (rawText ['{'])


-- TOOLS

word : Parser (String, SourceMap)
word =
   Parser.succeed identity
     |= getChompedString (Parser.chompUntil (Parser.Token " " ExpectingEndOfWordSpace))

loop : state -> (state -> Step state a) -> a
loop s nextState =
    case nextState s of
        Loop s_ -> loop s_ nextState
        Done b -> b

type Step state a
    = Loop state
    | Done a


{-| Apply a parser zero or more times and return a list of the results.
-}
many :  Parser a -> Parser (List a)
many p =
    Parser.loop [] (manyHelp p)

manyHelp :  Parser a -> List a -> Parser (Parser.Step (List a) (List a))
manyHelp p vs =
    Parser.oneOf
        [ eof |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        , Parser.succeed (\v -> Parser.Loop (v :: vs))
            |= p
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        ]

eof : Parser ()
eof = Parser.end EndOfInput

-- ROUND TRIP TESTING

