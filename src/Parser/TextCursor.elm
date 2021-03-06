module Parser.TextCursor exposing (TextCursor, init)

{-| TextCursor is the data structure used by Parser.parseLoop:

@docs TextCursor, init

-}

import Parser.Expression exposing (Expression)


{-| Data structure used by Parser.parseLoop as it progressively "eats" bites of
the text, accumulating the parsed bites in the list `parsed: List Expression`.
The `offset` represents the position of beginning of the current `text` in the
original text. It is used to properly construct Expressions, which contain
as component a SourceMap, which locates the bite of text in the original input
text. .
-}
type alias TextCursor =
    { text : String
    , block : String
    , chunkNumber : Int
    , parsed : List Expression
    , stack : List String
    , offset : Int
    , count : Int
    , generation : Int
    }


empty : TextCursor
empty =
    { count = 0, text = "", block = "", chunkNumber = 0, parsed = [], stack = [], offset = 0, generation = 0 }


{-| Return a TextCursor with given chunkNumber and text
-}
init : Int -> Int -> String -> TextCursor
init generation initiaChunkNumber text =
    { count = 0, text = text, block = "", chunkNumber = initiaChunkNumber, parsed = [], stack = [], offset = 0, generation = generation }


{-| Append raw text to the current cursor.
-}
addText : String -> TextCursor -> TextCursor
addText newText cursor =
    { cursor | text = cursor.text ++ newText }
