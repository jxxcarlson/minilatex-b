module Parser.LParser exposing (..)


   --Expression(..)
   --, roundTripCheck
   --, expression
   --, expressionList)

import Parser.Advanced as Parser exposing ((|.), (|=))

import Parser.LExpression as Expression exposing(Expression(..), Problem(..), SourceMap)

import Parser.TextCursor as TextCursor exposing(TextCursor)

type alias Parser a = Parser.Parser Context Problem a

type Context = Foo | Bar





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
  Parser.map (\(t,s) -> Text t s)  (rawText2 stopChars)

--rawText : List Char -> Parser (String, {first : Int,  last : Int})
--rawText stopChars =
--  (\s -> Parser.succeed  (s, {first = 0,  last = 0}))
--      <| Parser.getChompedString <|
--        Parser.succeed ()
--          |. Parser.chompWhile (\c -> not (List.member c stopChars))

rawText_ : List Char -> Parser String
rawText_ stopChars =
  Parser.getChompedString <|
        Parser.succeed ()
          |. Parser.chompWhile (\c -> not (List.member c stopChars))

rawText2 : List Char -> Parser (String, SourceMap)
rawText2 stopChars =
  getChompedString2 <|
        Parser.succeed ()
          |. Parser.chompWhile (\c -> not (List.member c stopChars))


getChompedString2 : Parser a -> Parser (String, SourceMap)
getChompedString2 parser =
  Parser.succeed (\first_ last_ source_ -> (String.slice first_ last_ source_, {first = first_, last = last_, offset = 0}))
    |= Parser.getOffset
    |. parser
    |= Parser.getOffset
    |= Parser.getSource

-- MATH

inlineMath : Parser Expression
inlineMath =
    Parser.succeed (\(s, t) -> InlineMath s  {t | first = t.first - 1, last = t.last + 1})
      |. Parser.symbol (Parser.Token "$" ExpectingLeadingDollarSign)
      |= getChompedString2 (Parser.chompUntil (Parser.Token "$" ExpectingTrailingDollarSign1))
      |. Parser.symbol (Parser.Token "$" ExpectingTrailingDollarSign2)
      |. Parser.spaces

displayMath : Parser Expression
displayMath =
    Parser.succeed (\(s,t) -> DisplayMath s  {t | first = t.first - 2, last = t.last + 2})
      |. Parser.symbol (Parser.Token "$$" ExpectingLeadingDoubleDollarSign)
      |= getChompedString2 (Parser.chompUntil (Parser.Token "$$" ExpectingLTrailingDoubleDollarSign1))
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
     |= getChompedString2 (Parser.chompUntil (Parser.Token " " ExpectingEndOfWordSpace))


textLoop : String -> TextCursor
textLoop str =
    loop (TextCursor.init str) nextRound

nextRound : TextCursor -> Step TextCursor TextCursor
nextRound tc =
    if tc.text == "" then Done tc
    else
        case Parser.run expression tc.text of
            Ok expr ->
                let
                   sourceMap = Expression.getSource expr
                   newText = String.dropLeft sourceMap.last tc.text
                   newExpr = Expression.incrementOffset tc.offset expr
                in
                Loop { tc| text = newText, parsed = newExpr::tc.parsed, offset = tc.offset + sourceMap.last}
            Err e ->
                Loop (handleError tc e)


handleError tc_ e =
    let
       mFirstError = e |> List.head
       errorColumn =
          mFirstError |> Maybe.map .col |> Maybe.withDefault 0
       errorText = String.left errorColumn tc_.text
       errorColumn2 = case Parser.run word errorText of
            Ok (_,t) -> t.last
            Err err -> 4
       errorText2 = String.left errorColumn2 errorText
       newText = String.dropLeft errorColumn2 tc_.text
    in
    { text = newText
     , parsed = (Text ("Error=[" ++ errorText2 ++ "]") {first = 0, last = errorColumn2, offset = tc_.offset + errorColumn2}) :: tc_.parsed
     , stack = errorText :: tc_.stack
     , offset = tc_.offset + errorColumn}



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


roundTrip : String -> String
roundTrip str =
    case Parser.run expressionList str of
        Ok r -> List.map toString r |> String.join " "
        Err err -> Debug.toString err


squeeze : String -> String
squeeze str =
    str |> String.replace " " "" |> String.replace "\n" ""

roundTripCheck : String -> Bool
roundTripCheck str =
    squeeze str == squeeze (roundTrip str)

toString : Expression -> String
toString expr =
    case expr of
        Text str _ -> str
        InlineMath str _ -> "$" ++ str ++ "$"
        DisplayMath str _ -> str
        LXList list -> List.foldl (\e acc -> acc ++ toString e) "" list


toStringFromList : List Expression -> String
toStringFromList list =
    list
      |> List.map toString
      |> String.join " "

parseAndRecompose : String -> String
parseAndRecompose str =
    str |> textLoop |> .parsed |> List.reverse |> toStringFromList


-- FUN STUFF
-- The below are not needed, but were fun to make and play around with

second : Parser a -> Parser a -> Parser a
second p q  =
    p |> Parser.andThen (\_ -> q)

first : Parser a -> Parser a -> Parser a
first p q  =
     p |> Parser.andThen (\s -> q |> Parser.map (\_ -> s) )

two : Parser a -> Parser a -> Parser (List a)
two p q  =
    p |> Parser.andThen (\s -> q |> Parser.map (\t -> [s, t]))

