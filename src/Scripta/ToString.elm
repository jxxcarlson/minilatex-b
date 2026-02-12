module Scripta.ToString exposing (..)

import Either exposing (Either(..))
import Scripta.FromLaTeX
import Scripta.Types exposing (Expr(..), Expression, ExpressionBlock, Heading(..))


fromString : String -> String
fromString str =
    str |> Scripta.FromLaTeX.convertFromString |> fromAST


fromAST : List ExpressionBlock -> String
fromAST blocks =
    List.map blockToString blocks |> String.join "\n\n"


blockToString : ExpressionBlock -> String
blockToString block =
    case block.heading of
        Paragraph ->
            bodyToString block.body

        Verbatim "math" ->
            "$$\n" ++ leftBodyString block.body ++ "\n$$"

        Verbatim name ->
            "| " ++ name ++ "\n" ++ leftBodyString block.body

        Ordinary name ->
            let
                content =
                    bodyToString block.body |> String.trim
            in
            "| " ++ name ++ "\n" ++ content ++ "\n"


bodyToString : Either String (List Expression) -> String
bodyToString body =
    case body of
        Left str ->
            str

        Right exprs ->
            exprListToString exprs


leftBodyString : Either String (List Expression) -> String
leftBodyString body =
    case body of
        Left str ->
            str

        Right _ ->
            ""


exprListToString : List Expression -> String
exprListToString exprs =
    List.map exprToString exprs |> String.join ""


exprToString : Expression -> String
exprToString expr =
    case expr of
        Text str _ ->
            str

        VFun "$" str _ ->
            "$" ++ str ++ "$"

        VFun "$$" str _ ->
            "$$\n" ++ str ++ "\n$$"

        VFun name str _ ->
            name ++ str

        Fun name args _ ->
            "[" ++ name ++ " " ++ exprListToString args ++ "]"

        ExprList _ exprs _ ->
            exprListToString exprs
