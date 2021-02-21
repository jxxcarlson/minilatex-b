module Parser.LXExpression exposing (
   LXExpression(..)
   , roundTripCheck
   , expression
   , expressionList)

import Parser.Advanced as Parser exposing ((|.), (|=))

type LXExpression
    = Text String
    | InlineMath String
    | DisplayMath String
    | LXList (List LXExpression)

type alias Parser a = Parser.Parser Context Problem a


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




toString : LXExpression -> String
toString expr =
    case expr of
        Text str -> str
        InlineMath str -> "$" ++ str ++ "$"
        DisplayMath str -> str
        LXList list -> List.foldl (\e acc -> acc ++ toString e) "" list



type Context = Foo | Bar

type Problem =
    ExpectingLeadingDollarSign
   | ExpectingTrailingDollarSign
   | ExpectingComma
   | ExpectingTextChar
   | ExpectingInt
   | InvalidNumber
   | EndOfInput


{-|

> run expressionList "foo bar $a^2$ baz"
Ok [Text ("foo bar "),InlineMath "a^2",Text "baz"]

-}
expressionList : Parser (List LXExpression)
expressionList = many expression

expression : Parser.Parser Context Problem LXExpression
expression = Parser.oneOf [displayMath, inlineMath, text]

eof : Parser ()
eof = Parser.end EndOfInput

text : Parser LXExpression
text = text_ ['$', '\\']

text_ : List Char -> Parser LXExpression
text_ stopChars =
  Parser.map Text <| Parser.getChompedString <|
    Parser.succeed ()
      |. Parser.chompWhile (\c -> not (List.member c stopChars))

inlineMath : Parser LXExpression
inlineMath =
    Parser.succeed InlineMath
      |. Parser.symbol (Parser.Token "$" ExpectingLeadingDollarSign)
      |= Parser.getChompedString (Parser.chompUntil (Parser.Token "$" ExpectingTrailingDollarSign))
      |. Parser.symbol (Parser.Token "$" ExpectingLeadingDollarSign)
      |. Parser.spaces

displayMath : Parser LXExpression
displayMath =
    Parser.succeed DisplayMath
      |. Parser.symbol (Parser.Token "$$" ExpectingLeadingDollarSign)
      |= Parser.getChompedString (Parser.chompUntil (Parser.Token "$$" ExpectingTrailingDollarSign))
      |. Parser.symbol (Parser.Token "$$" ExpectingLeadingDollarSign)
      |. Parser.spaces


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






-- FUN STUFF
-- The below are not needed, but were fun to make and play around with

second : Parser a -> Parser a -> Parser a
second p q  =
    p |> Parser.andThen (\_ -> q)

first : Parser a -> Parser a -> Parser a
first p q  =
     p |> Parser.andThen (\s -> q |> Parser.map (\_ -> s) )

combine : Parser a -> Parser a -> Parser (List a)
combine p q  =
    p |> Parser.andThen (\s -> q |> Parser.map (\t -> [s, t]))


int2 =
    Parser.succeed identity
       |= Parser.int ExpectingInt InvalidNumber
       |. Parser.symbol (Parser.Token  "," ExpectingComma)