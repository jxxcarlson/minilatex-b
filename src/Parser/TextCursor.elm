module Parser.TextCursor exposing (..)

import Parser.LExpression exposing(Expression)

type alias TextCursor =
    { text : String
    , parsed : List Expression
    , stack : List String
    , offset : Int
    --, annotationStack : List Annotation
    }


empty : TextCursor
empty = { text = "", parsed = [], stack = [], offset = 0}

init : String -> TextCursor
init str = { text = str, parsed = [], stack = [], offset = 0}

--type alias Annotation =
--    { startMark : Loc String
--    , expectedEndMark : String
--    , commandOccursAfterwards : Occurs
--    , precedingText : List Expression
--    }


{-| Append raw text to the current cursor.
-}
addText : String -> TextCursor -> TextCursor
addText newText cursor =
    { cursor | text = cursor.text ++ newText }

