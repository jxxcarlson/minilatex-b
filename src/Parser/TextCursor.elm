module Parser.TextCursor exposing (TextCursor, init)

{-| TextCursor is the data structure used by Parser.parseLoop:

@docs TextCursor, init

-}

import Parser.Expression exposing (Expression)


{-| Data structure used by Parser.parseLoop
-}
type alias TextCursor =
    { text : String
    , chunkNumber : Int
    , parsed : List Expression
    , stack : List String
    , offset : Int
    , count : Int
    }


empty : TextCursor
empty =
    { count = 0, text = "", chunkNumber = 0, parsed = [], stack = [], offset = 0 }


{-| Return a TextCursor with given chunkNumber and text
-}
init : Int -> String -> TextCursor
init initiaChunkNumber text =
    { count = 0, text = text, chunkNumber = initiaChunkNumber, parsed = [], stack = [], offset = 0 }


{-| Append raw text to the current cursor.
-}
addText : String -> TextCursor -> TextCursor
addText newText cursor =
    { cursor | text = cursor.text ++ newText }
