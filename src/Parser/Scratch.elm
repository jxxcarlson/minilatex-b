module Parser.Scratch exposing (..)

import Parser exposing((|.), (|=))

--import Parser.Advanced as Parser
--
--type alias Parser a = Parser.Parser Context Problem a
--

type BlockType = Undefined | TextBlock | MathBlock | EnvBlock String

blocktypeParser = Parser.oneOf [mathBlockParser, envLineParser, textBlockParser, Parser.succeed Undefined]

envLineParser : Parser.Parser BlockType
envLineParser =
   Parser.succeed (\s -> EnvBlock s)
      |. Parser.symbol "\\begin{"
      |= Parser.getChompedString (Parser.chompUntil "}")

mathBlockParser : Parser.Parser BlockType
mathBlockParser =
    Parser.succeed  MathBlock
      |. Parser.symbol "$$"

textBlockParser : Parser.Parser BlockType
textBlockParser =
    Parser.succeed TextBlock
      |. Parser.chompIf (\c -> True)

-- FUN STUFF
-- The below are not needed, but were fun to make and play around with

--second : Parser a -> Parser a -> Parser a
--second p q  =
--    p |> Parser.andThen (\_ -> q)
--
--first : Parser a -> Parser a -> Parser a
--first p q  =
--     p |> Parser.andThen (\s -> q |> Parser.map (\_ -> s) )
--
--two : Parser a -> Parser a -> Parser (List a)
--two p q  =
--    p |> Parser.andThen (\s -> q |> Parser.map (\t -> [s, t]))

