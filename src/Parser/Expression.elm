module Parser.Expression exposing (..)


type Expression
    = Text String SourceMap
    | InlineMath String SourceMap
    | DisplayMath String SourceMap
    | Macro String (Maybe String) (List Expression) SourceMap
    | LXList (List Expression)
    | LXError String Problem SourceMap
    | LXNull () SourceMap


type alias SourceMap =
    { lineNumber : Int, length : Int, offset : Int }


type alias Slice =
    { left : String, middle : String, right : String }


sliceWithSourceMap1 : SourceMap -> String -> Slice
sliceWithSourceMap1 sm str =
    slice sm.offset (sm.offset + sm.length) str


sliceWithSourceMap : SourceMap -> String -> Slice
sliceWithSourceMap sm str =
    slice (sm.offset - sm.length) sm.offset str


slice : Int -> Int -> String -> Slice
slice cut1 cut2 str =
    let
        middle =
            String.slice cut1 cut2 str

        left =
            String.left cut1 str

        right =
            String.dropLeft cut2 str
    in
    { left = left, middle = middle, right = right }


sourceMapToString : SourceMap -> String
sourceMapToString sm =
    "{ line = "
        ++ String.fromInt sm.lineNumber
        ++ ", offset = "
        ++ String.fromInt sm.offset
        ++ ", length = "
        ++ String.fromInt sm.length
        ++ "}"


toString : Expression -> String
toString expr =
    case expr of
        Text str _ ->
            str

        InlineMath str _ ->
            "$" ++ str ++ "$"

        DisplayMath str _ ->
            "$$" ++ str ++ "$$"

        Macro name optArg args str ->
            "\\" ++ name ++ Maybe.withDefault "OptArg: Null" optArg ++ (List.map toString args |> String.join "")

        LXError str p sm ->
            "((( Error at " ++ String.fromInt sm.offset ++ ": " ++ problemAsString p ++ " [" ++ str ++ "]  )))"

        LXList list ->
            List.foldl (\e acc -> acc ++ toString e) "" list

        LXNull () _ ->
            " "


getSourceOfList : List Expression -> SourceMap
getSourceOfList list =
    let
        sourceMaps =
            List.map getSource list

        length =
            List.maximum (List.map .length sourceMaps) |> Maybe.withDefault 0

        offset =
            List.minimum (List.map .offset sourceMaps) |> Maybe.withDefault 0

        lineNumber =
            List.head sourceMaps |> Maybe.map .lineNumber |> Maybe.withDefault 0
    in
    { length = length, offset = offset, lineNumber = lineNumber }


getSource : Expression -> SourceMap
getSource expr =
    case expr of
        Text _ source ->
            source

        InlineMath _ source ->
            source

        DisplayMath _ source ->
            source

        Macro _ _ _ source ->
            source

        LXError _ _ source ->
            source

        LXList e ->
            List.map getSource e |> List.head |> Maybe.withDefault { lineNumber = -1, length = -1, offset = -1 }

        LXNull _ source ->
            source


incrementOffset : Int -> Expression -> Expression
incrementOffset delta expr =
    case expr of
        Text e source ->
            Text e { source | offset = source.offset + delta }

        InlineMath e source ->
            InlineMath e { source | offset = source.offset + delta }

        DisplayMath e source ->
            DisplayMath e { source | offset = source.offset + delta }

        Macro n o a source ->
            Macro n o a { source | offset = source.offset + delta }

        LXError e p source ->
            LXError e p { source | offset = source.offset + delta }

        LXList e ->
            LXList (List.map (incrementOffset delta) e)

        LXNull () source ->
            LXNull () { source | offset = source.offset + delta }


type Problem
    = ExpectingLeadingDollarSign
    | ExpectingTrailingDollarSign1
    | ExpectingTrailingDollarSign2
    | ExpectingLeadingDoubleDollarSign
    | ExpectingLTrailingDoubleDollarSign1
    | ExpectingLTrailingDoubleDollarSign2
    | EndOfInput
    | ExpectingEndOfWordSpace
    | ExpectingLeftBracketForOptArg
    | ExpectingRightBracketForOptArg
    | ExpectingLeadingBackslashForMacro
    | ExpectingLeftBraceForArg
    | ExpectingRightBraceForArg
    | GenericError
    | ExpectingPrefix Char
    | ExpectingSpace


problemAsString : Problem -> String
problemAsString prob =
    case prob of
        ExpectingLeadingDollarSign ->
            "Unmatched '$' (1)"

        ExpectingTrailingDollarSign1 ->
            -- EDITED
            "Unmatched '$' (2)"

        ExpectingTrailingDollarSign2 ->
            "Unmatched '$' (3)"

        ExpectingLeadingDoubleDollarSign ->
            "Unmatched '$$' (4)"

        ExpectingLTrailingDoubleDollarSign1 ->
            "Unmatched '$$' (5)"

        ExpectingLTrailingDoubleDollarSign2 ->
            "Unamtched '$$' (6)"

        EndOfInput ->
            "Unexpected end of input (7)"

        ExpectingLeadingBackslashForMacro ->
            "Expecting '\\' to begin a macro (8)"

        ExpectingLeftBracketForOptArg ->
            "Expecting a left bracket (9)"

        ExpectingRightBracketForOptArg ->
            "Expecting a right bracket (10)"

        GenericError ->
            "Generic error (11)s"

        ExpectingEndOfWordSpace ->
            "Expecting space (12)"

        ExpectingLeftBraceForArg ->
            "Expecting brace (13)"

        ExpectingRightBraceForArg ->
            "Missing: }"

        ExpectingPrefix c ->
            "Expecting prefix " ++ String.fromChar c ++ " (15)"

        ExpectingSpace ->
            "Expecting space (16)"
