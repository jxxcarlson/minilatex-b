module Parser.Expression exposing
    ( Expression(..), incrementBlockOffset, incrementOffset, toString
    , SourceMap, dummySourceMap, getSelectionFromSourceMap, getSource, getSourceOfList, sourceMapIndex, sourceMapToString, setSourceMap
    , Problem(..), equivalentProblem, problemAsString
    , Context(..), makeSourceMap
    )

{-| Module **Parser.Expression** defines various data structures used by the parser (module Parser.Parser).
Chief among them is _Expression_, which is the type of the syntax tree. An expression contains both
syntax information and a _SourceMap_, which locates the part of the source text from which that element
was derived. This is useful in interactive editing apps.


## Expression

@docs Expression, incrementBlockOffset, incrementOffset, toString


## SourceMap

@docs SourceMap, dummySourceMap, getSelectionFromSourceMap, getSource, getSourceOfList, sourceMapIndex, sourceMapToString, setSourceMap


## Problem

@docs Problem, equivalentProblem, problemAsString

-}

import List.Extra


{-| The type of the MiniLaTeX AST
-}
type Expression
    = Text String SourceMap
    | Comment String SourceMap
    | Item Int Expression SourceMap
    | InlineMath String SourceMap
    | DisplayMath String SourceMap
    | Macro String (Maybe Expression) (List Expression) SourceMap
    | Environment String (List Expression) Expression SourceMap -- Environment name optArgs body
    | NewCommand String Int Expression SourceMap
    | LXList (List Expression)
    | LXError String Problem SourceMap
    | LXInstruction Instr SourceMap


type Instr
    = INoOp
    | IHighlight


instructionToString : Instr -> String
instructionToString i =
    case i of
        INoOp ->
            "NoOp"

        IHighlight ->
            "Highlight"


{-| Identifies the source text corresponding to part of the AST
-}
type alias SourceMap =
    { blockOffset : Int
    , length : Int
    , offset : Int
    , content : String
    , generation : Int
    }


makeSourceMap : Int -> Int -> Int -> Int -> String -> SourceMap
makeSourceMap gen bo start fin src =
    { blockOffset = bo, generation = gen, offset = start, length = fin - start, content = src }


{-| -}
type Context
    = InlineMathContext
    | DisplayMathContext
    | MacroNameContext
    | OptArgContext
    | ArgContext
    | WordContext
    | EnvNameContext


{-| Used to identify parse errors
-}
type Problem
    = ExpectingLeadingDollarSign
    | ExpectingLeadingDoubleDollarSign
    | ExpectingLeadingPercentSign
    | ExpectingEndofLine
    | EndOfInput
    | ExpectingEndOfWordSpace
    | ExpectingEndWordInItemList String
    | ExpectingLeftBracket
    | ExpectingEscapedItem
    | ExpectingSpaceAfterItem
    | ExpectingRightBracket
    | ExpectingBackslash
    | ExpectingLeftBrace
    | ExpectingRightBrace
    | ExpectingTrailingDollarSign
    | ExpectingTrailingDoubleDollarSign
    | UnHandledError
    | ExpectingInt
    | InvalidInt
    | ExpectingValidMacroArgWord
    | ExpectingPrefix Char
    | ExpectingSpace
    | RejectMacroReservedWord
    | ExpectingBegin
    | ExpectingEndWord String
    | ExpectingValidOptionArgWord
    | ExpectingEndForPassThroughBody
    | ExpectingPrefixes (List Char)


{-| -}
equivalentProblem : Problem -> Problem -> Bool
equivalentProblem p1 p2 =
    case ( p1, p2 ) of
        ( ExpectingPrefix _, ExpectingPrefix _ ) ->
            True

        ( ExpectingEndWord _, ExpectingEndWord _ ) ->
            True

        ( ExpectingPrefixes _, ExpectingPrefixes _ ) ->
            True

        _ ->
            p1 == p2


type alias Slice =
    { left : String
    , middle : String
    , right : String
    }


{-| -}
dummySourceMap : SourceMap
dummySourceMap =
    { blockOffset = 0, length = 0, offset = 0, content = "", generation = 0 }


{-| Return the string in the source text identified by the SourceMap.
The auxiliary data structure SourceMapIndex: List (List Int),
typically computed by sourceMapIndex and stored in the model, is needed for this.
-}
getSelectionFromSourceMap : SourceMap -> String -> List (List Int) -> String
getSelectionFromSourceMap sourceMap str sourceMapIndex_ =
    let
        lines =
            String.lines str

        data : List Int
        data =
            linesOfChunkOffset sourceMap.blockOffset sourceMapIndex_
                |> List.map Tuple.second
                |> List.head
                |> Maybe.withDefault []

        sel_ : List String
        sel_ =
            List.foldl (\i acc -> (List.Extra.getAt i lines |> Maybe.withDefault "") :: acc) [] data

        selectedLines : String
        selectedLines =
            List.foldl (\l acc -> acc ++ "\n" ++ l) "" (List.reverse sel_)

        selection =
            String.slice (sourceMap.offset + 1) (sourceMap.offset + sourceMap.length + 1) selectedLines
    in
    selection


makeIndex : Int -> List Int -> List (List Int)
makeIndex numberOfLines list =
    let
        list2 =
            list
                |> List.filter (\x -> x /= 0)
                |> (\x -> 0 :: x)
                |> List.drop 1
                |> List.map (\x -> x - 2)
                |> (\x -> x ++ [ numberOfLines - 1 ])

        pairs =
            List.map2 (\x y -> ( x, y )) list list2
    in
    List.map (\( x, y ) -> List.range x y) pairs


{-| Compute the SourceMapIndex of an AST given the number of lines
in the input.
-}
sourceMapIndex : Int -> List (List Expression) -> List (List Int)
sourceMapIndex numberOfLines list =
    list
        |> List.map getSourceOfList
        |> List.map .blockOffset
        |> makeIndex numberOfLines


indexedList : List a -> List ( Int, a )
indexedList list =
    let
        n =
            List.length list - 1
    in
    List.map2 (\k a -> ( k, a )) (List.range 0 n) list


indexedFilter : (a -> Bool) -> List a -> List ( Int, a )
indexedFilter predicate list =
    List.filter (\( _, a ) -> predicate a) (indexedList list)


linesOfChunkOffset : a -> List (List a) -> List ( Int, List a )
linesOfChunkOffset chunkOffset sourceMapIndex_ =
    indexedFilter (\chunks -> List.member chunkOffset chunks) sourceMapIndex_


{-| String representation of a SourceMap
-}
sourceMapToString : SourceMap -> String
sourceMapToString sm =
    "{ chunkOffset = "
        ++ String.fromInt sm.blockOffset
        ++ ", offset = "
        ++ String.fromInt sm.offset
        ++ ", length = "
        ++ String.fromInt sm.length
        ++ "}"


{-| -}
toString : Expression -> String
toString expr =
    case expr of
        Text str _ ->
            str

        Comment str _ ->
            str

        Item _ e _ ->
            toString e

        InlineMath str _ ->
            "$" ++ str ++ "$"

        DisplayMath str _ ->
            "$$" ++ str ++ "$$"

        Macro name optArg args _ ->
            let
                optArgAsString =
                    Maybe.map toString optArg |> Maybe.withDefault "nada"
            in
            "\\" ++ name ++ "[" ++ optArgAsString ++ "]" ++ (List.map toString args |> String.join "")

        Environment name _ _ _ ->
            -- TODO: incomplete
            "\\begin{" ++ name ++ "} ... \\end{" ++ name ++ "}"

        NewCommand name _ e _ ->
            -- TODO: incomplete
            "\\newcommand: " ++ toString e

        LXError str p sm ->
            "((( Error at " ++ String.fromInt sm.offset ++ ": " ++ problemAsString p ++ " [" ++ str ++ "]  )))"

        LXList list ->
            List.foldl (\e acc -> acc ++ toString e) "" list

        LXInstruction instr _ ->
            instructionToString instr


{-| Return a SourceMap for a list of Expression.
-}
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
            List.head sourceMaps |> Maybe.map .blockOffset |> Maybe.withDefault 0

        content =
            List.head sourceMaps |> Maybe.map .content |> Maybe.withDefault ""

        generation =
            List.head sourceMaps |> Maybe.map .generation |> Maybe.withDefault 0
    in
    { length = length, offset = offset, blockOffset = lineNumber, content = content, generation = generation }


{-| Return the SourceMap component of an Expression
-}
getSource : Expression -> SourceMap
getSource expr =
    case expr of
        Text _ source ->
            source

        Comment _ source ->
            source

        Item _ _ source ->
            source

        InlineMath _ source ->
            source

        DisplayMath _ source ->
            source

        Macro _ _ _ source ->
            source

        LXError _ _ source ->
            source

        Environment _ _ _ source ->
            source

        NewCommand _ _ _ source ->
            source

        LXList e ->
            List.map getSource e |> List.head |> Maybe.withDefault dummySourceMap

        LXInstruction _ source ->
            source


{-| increment the offset field of the SourceMap component of an Expression
-}
incrementOffset : Int -> Expression -> Expression
incrementOffset delta expr =
    case expr of
        Text e source ->
            Text e { source | offset = source.offset + delta }

        Comment e source ->
            Comment e { source | offset = source.offset + delta }

        Item k e source ->
            Item k e { source | offset = source.offset + delta }

        InlineMath e source ->
            InlineMath e { source | offset = source.offset + delta }

        DisplayMath e source ->
            DisplayMath e { source | offset = source.offset + delta }

        Macro n o a source ->
            Macro n o a { source | offset = source.offset + delta }

        Environment n a e source ->
            Environment n a e { source | offset = source.offset + delta }

        NewCommand n k e source ->
            NewCommand n k e { source | offset = source.offset + delta }

        LXError e p source ->
            LXError e p { source | offset = source.offset + delta }

        LXList e ->
            LXList (List.map (incrementOffset delta) e)

        LXInstruction instr source ->
            LXInstruction instr { source | offset = source.offset + delta }


{-| increment the blockOffset field of the SourceMap component of an Expression
-}
incrementBlockOffset : Int -> Expression -> Expression
incrementBlockOffset delta expr =
    case expr of
        Text e source ->
            Text e { source | blockOffset = source.blockOffset + delta }

        Comment e source ->
            Comment e { source | blockOffset = source.blockOffset + delta }

        Item k e source ->
            Item k e { source | blockOffset = source.blockOffset + delta }

        InlineMath e source ->
            InlineMath e { source | blockOffset = source.blockOffset + delta }

        DisplayMath e source ->
            DisplayMath e { source | blockOffset = source.blockOffset + delta }

        Macro n o a source ->
            Macro n o a { source | blockOffset = source.blockOffset + delta }

        Environment n a e source ->
            Environment n a e { source | blockOffset = source.blockOffset + delta }

        NewCommand n k e source ->
            NewCommand n k e { source | blockOffset = source.blockOffset + delta }

        LXError e p source ->
            LXError e p { source | blockOffset = source.blockOffset + delta }

        LXList e ->
            LXList (List.map (incrementOffset delta) e)

        LXInstruction instr source ->
            LXInstruction instr { source | blockOffset = source.blockOffset + delta }


{-| -}
setSourceMap : SourceMap -> Expression -> Expression
setSourceMap sm expr =
    case expr of
        Text e _ ->
            Text e sm

        Comment e _ ->
            Comment e sm

        Item k e _ ->
            Item k e sm

        InlineMath e _ ->
            InlineMath e sm

        DisplayMath e _ ->
            DisplayMath e sm

        Macro n o a _ ->
            Macro n o a sm

        Environment n a e _ ->
            Environment n a e sm

        NewCommand n k e _ ->
            NewCommand n k e sm

        LXError e p _ ->
            LXError e p sm

        LXList e ->
            LXList (List.map (setSourceMap sm) e)

        LXInstruction instr _ ->
            LXInstruction instr sm


{-| String representation of a Problem. Used in error reporting.
-}
problemAsString : Problem -> String
problemAsString prob =
    case prob of
        ExpectingLeadingDollarSign ->
            "Unmatched '$' (1)"

        ExpectingTrailingDollarSign ->
            "Unmatched '$' (2)"

        ExpectingLeadingDoubleDollarSign ->
            "Unmatched '$$' (4)"

        ExpectingTrailingDoubleDollarSign ->
            "Unmatched '$$' (5)"

        EndOfInput ->
            "Unexpected end of input (7)"

        ExpectingBackslash ->
            "Expecting '\\' to begin a macro (8)"

        ExpectingLeftBracket ->
            "Expecting a left bracket (9)"

        ExpectingRightBracket ->
            "Expecting right bracket (10)"

        UnHandledError ->
            "Generic error (11)s"

        ExpectingEndOfWordSpace ->
            "Expecting space (12)"

        ExpectingLeftBrace ->
            "Expecting left brace (13)"

        ExpectingRightBrace ->
            "Missing: }"

        ExpectingPrefix c ->
            "Expecting prefix " ++ String.fromChar c ++ " (15)"

        ExpectingSpace ->
            "Expecting space (16)"

        RejectMacroReservedWord ->
            "Expecting macro word (17)"

        ExpectingBegin ->
            "Expecting \\begin (18)"

        ExpectingEndWord str ->
            "Expecting " ++ str ++ " (19)"

        ExpectingValidOptionArgWord ->
            "Expecting valid option arg word (20)"

        ExpectingEndForPassThroughBody ->
            "Expecting end for pass-through body (21)"

        ExpectingPrefixes _ ->
            "Expecting prefixes ... (22)"

        ExpectingEndofLine ->
            "Expecting end of line (23)"

        ExpectingLeadingPercentSign ->
            "Expecting leading % ... (24)"

        ExpectingInt ->
            "Expecting Int (25)"

        InvalidInt ->
            "Invalid Int (26)"

        ExpectingValidMacroArgWord ->
            "Expecting valid macro arg word (27)"

        ExpectingEndWordInItemList _ ->
            "Expecting end word in item lsit (28)"

        ExpectingEscapedItem ->
            "Expecting escaped item (29)"

        ExpectingSpaceAfterItem ->
            "Expecting space after item (30)"
