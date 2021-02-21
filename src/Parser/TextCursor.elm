module Parser.TextCursor exposing (..)

import Parser.LExpression exposing(Expression)

type alias TextCursor =
    { text : String
    , parsed : List Expression
    --, annotationStack : List Annotation
    }


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

