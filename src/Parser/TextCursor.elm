module Parser.TextCursor exposing (..)

import Parser.Expression exposing (Expression)


type alias TextCursor =
    { text : String
    , chunkNumber : Int
    , parsed : List Expression
    , stack : List String
    , offset : Int
    }


empty : TextCursor
empty =
    { text = "", chunkNumber = 0, parsed = [], stack = [], offset = 0 }


init : Int -> String -> TextCursor
init initiaChunkNumber str =
    { text = str, chunkNumber = initiaChunkNumber, parsed = [], stack = [], offset = 0 }


{-| Append raw text to the current cursor.
-}
addText : String -> TextCursor -> TextCursor
addText newText cursor =
    { cursor | text = cursor.text ++ newText }
