module Parser.Expression exposing (..)


type Expression
    = Text String SourceMap
    | InlineMath String SourceMap
    | DisplayMath String SourceMap
    | Macro String (Maybe String) (List String) SourceMap
    | LXList (List Expression)
    | LXError String Problem SourceMap


type alias SourceMap =
    { lineNumber : Int, length : Int, offset : Int }


type alias Slice =
    { left : String, middle : String, right : String }


sliceWithSourceMap : SourceMap -> String -> Slice
sliceWithSourceMap sm str =
    slice sm.offset (sm.offset + sm.length) str


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
            "\\" ++ name ++ Maybe.withDefault "OptArg: Null" optArg ++ (args |> String.join "")

        LXError str p sm ->
            "((( Error at " ++ String.fromInt sm.offset ++ ": " ++ problemAsString p ++ " [" ++ str ++ "]  )))"

        LXList list ->
            List.foldl (\e acc -> acc ++ toString e) "" list


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


problemAsString : Problem -> String
problemAsString prob =
    case prob of
        ExpectingLeadingDollarSign ->
            "I was expecting a '$' to begin inline math"

        ExpectingTrailingDollarSign1 ->
            -- EDITED
            "Unmatched '$'"

        ExpectingTrailingDollarSign2 ->
            "I was expecting a matching '$' to end inline math (2)"

        ExpectingLeadingDoubleDollarSign ->
            "I was expecting '$$' to begin display math"

        ExpectingLTrailingDoubleDollarSign1 ->
            "I was expecting a matching '$$' to end display math (1)"

        ExpectingLTrailingDoubleDollarSign2 ->
            "I was expecting a matching '$$' to end display math (2)"

        EndOfInput ->
            "Unexpected end of input"

        ExpectingLeadingBackslashForMacro ->
            "I was expecting '\\' to begin a macro"

        ExpectingLeftBracketForOptArg ->
            "I was expecting a left bracket for optional argument"

        ExpectingRightBracketForOptArg ->
            "I was expecting a right bracket for optional argument"

        GenericError ->
            "Generic error -- I'm at a loss for wards"

        ExpectingEndOfWordSpace ->
            "Expecting a space to end a waord"

        ExpectingLeftBraceForArg ->
            "Expecting left brace for macro arg"

        ExpectingRightBraceForArg ->
            "Expecting right brace for macro arg"
