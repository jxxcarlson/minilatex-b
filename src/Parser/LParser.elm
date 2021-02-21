module Parser.LParser exposing (..)


   --Expression(..)
   --, roundTripCheck
   --, expression
   --, expressionList)

import Parser.Advanced as Parser exposing ((|.), (|=))

import Parser.LExpression exposing(Expression(..), Problem(..))

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
  Parser.map Text (rawText stopChars)

rawText : List Char -> Parser String
rawText stopChars =
  Parser.getChompedString <|
    Parser.succeed ()
      |. Parser.chompWhile (\c -> not (List.member c stopChars))


-- MATH

inlineMath : Parser Expression
inlineMath =
    Parser.succeed InlineMath
      |. Parser.symbol (Parser.Token "$" ExpectingLeadingDollarSign)
      |= Parser.getChompedString (Parser.chompUntil (Parser.Token "$" ExpectingTrailingDollarSign1))
      |. Parser.symbol (Parser.Token "$" ExpectingTrailingDollarSign2)
      |. Parser.spaces

displayMath : Parser Expression
displayMath =
    Parser.succeed DisplayMath
      |. Parser.symbol (Parser.Token "$$" ExpectingLeadingDoubleDollarSign)
      |= Parser.getChompedString (Parser.chompUntil (Parser.Token "$$" ExpectingLTrailingDoubleDollarSign1))
      |. Parser.symbol (Parser.Token "$$" ExpectingLTrailingDoubleDollarSign2)
      |. Parser.spaces


-- MACRO

--macro : Parser Expression
--macro =
--    Parser.succeed Macro

macroName : Parser String
macroName =
    Parser.succeed identity
      |. Parser.symbol (Parser.Token "\\" ExpectingLeadingBackslashForMacro)
      |= (rawText ['{'])


-- TOOLS


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
        Text str -> str
        InlineMath str -> "$" ++ str ++ "$"
        DisplayMath str -> str
        LXList list -> List.foldl (\e acc -> acc ++ toString e) "" list





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

