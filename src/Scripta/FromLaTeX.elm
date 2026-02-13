module Scripta.FromLaTeX exposing (convert, convertFromString)

import Dict
import Either exposing (Either(..))
import MiniLaTeX
import Parser.Expression as PE exposing (Expression(..), SourceMap)
import Scripta.Types


{-|

    convertFromString "\\italic{stuff}"
    [{
      ....args = []
    , body = Right [Fun "italic" [Text "stuff" { begin = 0, end = 13, id = "0:0-0", index = 0 }]
      { begin = 0, end = 14, id = "0:0-0", index = 0 }]
    , firstLine = "\\italic[nada]stuff"
    , heading = Paragraph
    , indent = 0
    , meta = { bodyLineNumber = 1
    , error = Nothing, id = "0:0"
    , lineNumber = 0
    , messages = []
    , numberOfLines = 1
    , position = 0
    , sourceText = "\\italic[nada]stuff" }
    , properties = Dict.fromList [], style = {}
    }]

-}
convertFromString : String -> List Scripta.Types.ExpressionBlock
convertFromString str =
    str |> MiniLaTeX.parse 0 |> convert


convert : List (List Expression) -> List Scripta.Types.ExpressionBlock
convert ast =
    List.indexedMap convertBlock ast
        |> List.filterMap identity



-- BLOCK CONVERSION


convertBlock : Int -> List Expression -> Maybe Scripta.Types.ExpressionBlock
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
                { heading = Scripta.Types.Verbatim "math"
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
                    { heading = Scripta.Types.Verbatim (passThroughToVerbatimName name)
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
                    { heading = Scripta.Types.Ordinary name
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
                { heading = Scripta.Types.Ordinary "maketitle"
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
                { heading = Scripta.Types.Paragraph
                , indent = 0
                , args = []
                , properties = Dict.empty
                , firstLine = firstLineFromExprs filtered
                , body = Right (convertExprList filtered)
                , meta = toBlockMeta blockIndex filtered
                , style = {}
                }


sectionBlock : Int -> String -> List Expression -> List Expression -> Scripta.Types.ExpressionBlock
sectionBlock blockIndex level args allExprs =
    { heading = Scripta.Types.Ordinary "section"
    , indent = 0
    , args = [ level ]
    , properties = Dict.empty
    , firstLine = "\\section"
    , body = Right (convertExprList args)
    , meta = toBlockMeta blockIndex allExprs
    , style = {}
    }


macroBlock : Int -> String -> List String -> List Expression -> List Expression -> Scripta.Types.ExpressionBlock
macroBlock blockIndex name extraArgs args allExprs =
    { heading = Scripta.Types.Ordinary name
    , indent = 0
    , args = extraArgs
    , properties = Dict.empty
    , firstLine = "\\" ++ name
    , body = Right (convertExprList args)
    , meta = toBlockMeta blockIndex allExprs
    , style = {}
    }



-- EXPRESSION CONVERSION


convertExpr : Int -> Expression -> Maybe Scripta.Types.Expression
convertExpr index expr =
    case expr of
        Text str sm ->
            Just (Scripta.Types.Text str (toExprMeta index sm))

        InlineMath str sm ->
            Just (Scripta.Types.VFun "math" str (toExprMeta index sm)) |> Debug.log "@MATH"

        DisplayMath str sm ->
            Just (Scripta.Types.VFun "$$" str (toExprMeta index sm))

        Macro name Nothing args sm ->
            Just (Scripta.Types.Fun name (convertExprList args) (toExprMeta index sm))

        Macro name (Just opt) args sm ->
            Just (Scripta.Types.Fun name (convertExprList (opt :: args)) (toExprMeta index sm))

        Item _ body sm ->
            Just (Scripta.Types.Fun "item" (convertExprList [ body ]) (toExprMeta index sm))

        Environment name _ body sm ->
            Just (Scripta.Types.Fun name (convertBody body) (toExprMeta index sm))

        LXList exprs ->
            Just (Scripta.Types.ExprList 0 (convertExprList exprs) dummyExprMeta)

        LXError str prob sm ->
            Just (Scripta.Types.Text ("Error: " ++ PE.problemAsString prob) (toExprMeta index sm))

        Comment _ _ ->
            Nothing

        NewCommand _ _ _ _ ->
            Nothing

        LXInstruction _ _ ->
            Nothing


convertExprList : List Expression -> List Scripta.Types.Expression
convertExprList exprs =
    List.indexedMap convertExpr exprs
        |> List.filterMap identity


convertBody : Expression -> List Scripta.Types.Expression
convertBody body =
    case body of
        LXList exprs ->
            convertExprList exprs

        _ ->
            convertExprList [ body ]



-- METADATA


toExprMeta : Int -> SourceMap -> Scripta.Types.ExprMeta
toExprMeta index sm =
    { begin = sm.offset
    , end = sm.offset + sm.length
    , index = index
    , id = String.fromInt sm.generation ++ ":" ++ String.fromInt sm.blockOffset ++ "-" ++ String.fromInt sm.offset
    }


dummyExprMeta : Scripta.Types.ExprMeta
dummyExprMeta =
    { begin = 0, end = 0, index = 0, id = "0:0-0" }


toBlockMeta : Int -> List Expression -> Scripta.Types.BlockMeta
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
    [ "equation"
    , "eqnarray"
    , "verbatim"
    , "colored"
    --, "CD" ???
    , "mathmacro"
    , "textmacro"
    , "listing"
    , "verse"
    , "align"
    --, "matrix" ???
    --, "pmatrix"
    --, "bmatrix"
    --, "Bmatrix"
    --, "vmatrix"
    --, "Vmatrix"
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
