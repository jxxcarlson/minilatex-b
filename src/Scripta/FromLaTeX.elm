module Scripta.FromLaTeX exposing (convert)

import Dict
import Either exposing (Either(..))
import Parser.Expression as PE exposing (Expression(..), SourceMap)
import Scripta.Types as ST


convert : List (List Expression) -> List ST.ExpressionBlock
convert ast =
    List.indexedMap convertBlock ast
        |> List.filterMap identity



-- BLOCK CONVERSION


convertBlock : Int -> List Expression -> Maybe ST.ExpressionBlock
convertBlock blockIndex exprs =
    let
        filtered =
            List.filter (not << isSkippable) exprs
    in
    case filtered of
        [] ->
            Nothing

        [ DisplayMath str sm ] ->
            Just
                { heading = ST.Verbatim "math"
                , indent = 0
                , args = []
                , properties = Dict.empty
                , firstLine = "$$"
                , body = Left str
                , meta = toBlockMeta blockIndex filtered
                , style = {}
                }

        [ Environment name optArgs body sm ] ->
            if isPassThroughEnv name then
                Just
                    { heading = ST.Verbatim (passThroughToVerbatimName name)
                    , indent = 0
                    , args = optArgsToStrings optArgs
                    , properties = Dict.empty
                    , firstLine = "\\begin{" ++ name ++ "}"
                    , body = Left (bodyToString body)
                    , meta = toBlockMeta blockIndex filtered
                    , style = {}
                    }

            else
                Just
                    { heading = ST.Ordinary name
                    , indent = 0
                    , args = optArgsToStrings optArgs
                    , properties = Dict.empty
                    , firstLine = "\\begin{" ++ name ++ "}"
                    , body = Right (convertBody body)
                    , meta = toBlockMeta blockIndex filtered
                    , style = {}
                    }

        [ Macro "section" _ args sm ] ->
            Just (sectionBlock blockIndex "1" args filtered)

        [ Macro "subsection" _ args sm ] ->
            Just (sectionBlock blockIndex "2" args filtered)

        [ Macro "subsubsection" _ args sm ] ->
            Just (sectionBlock blockIndex "3" args filtered)

        [ Macro "title" _ args sm ] ->
            Just (macroBlock blockIndex "title" [] args filtered)

        [ Macro "author" _ args sm ] ->
            Just (macroBlock blockIndex "author" [] args filtered)

        [ Macro "date" _ args sm ] ->
            Just (macroBlock blockIndex "date" [] args filtered)

        [ Macro "maketitle" _ _ sm ] ->
            Just
                { heading = ST.Ordinary "maketitle"
                , indent = 0
                , args = []
                , properties = Dict.empty
                , firstLine = "\\maketitle"
                , body = Right []
                , meta = toBlockMeta blockIndex filtered
                , style = {}
                }

        _ ->
            Just
                { heading = ST.Paragraph
                , indent = 0
                , args = []
                , properties = Dict.empty
                , firstLine = firstLineFromExprs filtered
                , body = Right (convertExprList filtered)
                , meta = toBlockMeta blockIndex filtered
                , style = {}
                }


sectionBlock : Int -> String -> List Expression -> List Expression -> ST.ExpressionBlock
sectionBlock blockIndex level args allExprs =
    { heading = ST.Ordinary "section"
    , indent = 0
    , args = [ level ]
    , properties = Dict.empty
    , firstLine = "\\section"
    , body = Right (convertExprList args)
    , meta = toBlockMeta blockIndex allExprs
    , style = {}
    }


macroBlock : Int -> String -> List String -> List Expression -> List Expression -> ST.ExpressionBlock
macroBlock blockIndex name extraArgs args allExprs =
    { heading = ST.Ordinary name
    , indent = 0
    , args = extraArgs
    , properties = Dict.empty
    , firstLine = "\\" ++ name
    , body = Right (convertExprList args)
    , meta = toBlockMeta blockIndex allExprs
    , style = {}
    }



-- EXPRESSION CONVERSION


convertExpr : Int -> Expression -> Maybe ST.Expression
convertExpr index expr =
    case expr of
        Text str sm ->
            Just (ST.Text str (toExprMeta index sm))

        InlineMath str sm ->
            Just (ST.VFun "$" str (toExprMeta index sm))

        DisplayMath str sm ->
            Just (ST.VFun "$$" str (toExprMeta index sm))

        Macro name Nothing args sm ->
            Just (ST.Fun name (convertExprList args) (toExprMeta index sm))

        Macro name (Just opt) args sm ->
            Just (ST.Fun name (convertExprList (opt :: args)) (toExprMeta index sm))

        Item _ body sm ->
            Just (ST.Fun "item" (convertExprList [ body ]) (toExprMeta index sm))

        Environment name _ body sm ->
            Just (ST.Fun name (convertBody body) (toExprMeta index sm))

        LXList exprs ->
            Just (ST.ExprList 0 (convertExprList exprs) dummyExprMeta)

        LXError str prob sm ->
            Just (ST.Text ("Error: " ++ PE.problemAsString prob) (toExprMeta index sm))

        Comment _ _ ->
            Nothing

        NewCommand _ _ _ _ ->
            Nothing

        LXInstruction _ _ ->
            Nothing


convertExprList : List Expression -> List ST.Expression
convertExprList exprs =
    List.indexedMap convertExpr exprs
        |> List.filterMap identity


convertBody : Expression -> List ST.Expression
convertBody body =
    case body of
        LXList exprs ->
            convertExprList exprs

        _ ->
            convertExprList [ body ]



-- METADATA


toExprMeta : Int -> SourceMap -> ST.ExprMeta
toExprMeta index sm =
    { begin = sm.offset
    , end = sm.offset + sm.length
    , index = index
    , id = String.fromInt sm.generation ++ ":" ++ String.fromInt sm.blockOffset ++ "-" ++ String.fromInt sm.offset
    }


dummyExprMeta : ST.ExprMeta
dummyExprMeta =
    { begin = 0, end = 0, index = 0, id = "0:0-0" }


toBlockMeta : Int -> List Expression -> ST.BlockMeta
toBlockMeta blockIndex exprs =
    let
        sm =
            PE.getSourceOfList exprs

        sourceText =
            List.map PE.toString exprs |> String.join " "
    in
    { id = String.fromInt sm.generation ++ ":" ++ String.fromInt sm.blockOffset
    , position = blockIndex
    , lineNumber = sm.blockOffset
    , bodyLineNumber = sm.blockOffset + 1
    , numberOfLines = List.length (String.lines sourceText)
    , messages = []
    , sourceText = sourceText
    , error = Nothing
    }



-- HELPERS


isSkippable : Expression -> Bool
isSkippable expr =
    case expr of
        Comment _ _ ->
            True

        NewCommand _ _ _ _ ->
            True

        LXInstruction _ _ ->
            True

        _ ->
            False


isPassThroughEnv : String -> Bool
isPassThroughEnv name =
    List.member name passThroughEnvList


passThroughEnvList : List String
passThroughEnvList =
    [ "equation", "eqnarray", "verbatim", "colored", "CD"
    , "mathmacro", "textmacro", "listing", "verse"
    , "align", "matrix", "pmatrix", "bmatrix", "Bmatrix", "vmatrix", "Vmatrix"
    ]


passThroughToVerbatimName : String -> String
passThroughToVerbatimName name =
    case name of
        "equation" ->
            "equation"

        "align" ->
            "aligned"

        "eqnarray" ->
            "aligned"

        "verbatim" ->
            "verbatim"

        "listing" ->
            "code"

        "colored" ->
            "code"

        "verse" ->
            "verse"

        "mathmacro" ->
            "mathmacros"

        "textmacro" ->
            "textmacros"

        -- matrix variants and CD → math
        _ ->
            "math"


optArgsToStrings : List Expression -> List String
optArgsToStrings optArgs =
    List.map PE.toString optArgs


bodyToString : Expression -> String
bodyToString body =
    case body of
        LXList exprs ->
            List.map PE.toString exprs |> String.join ""

        Text str _ ->
            str

        _ ->
            PE.toString body


firstLineFromExprs : List Expression -> String
firstLineFromExprs exprs =
    case exprs of
        [] ->
            ""

        first :: _ ->
            PE.toString first
                |> String.lines
                |> List.head
                |> Maybe.withDefault ""
