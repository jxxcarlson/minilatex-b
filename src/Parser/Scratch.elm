module Parser.Scratch exposing (..)


import Parser.Advanced as Parser

type alias Parser a = Parser.Parser Context Problem a


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

