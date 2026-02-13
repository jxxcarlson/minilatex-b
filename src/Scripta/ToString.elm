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

        Verbatim "mathmacros" ->
            let
                body =
                    leftBodyString block.body

                convertLine line =
                    case parseNewcommand (String.trim line) of
                        Just ( name, definition ) ->
                            Just (name ++ ": " ++ definition)

                        Nothing ->
                            if String.trim line == "" then
                                Nothing
                            else
                                Just line
            in
            "| mathmacros\n"
                ++ (String.lines body
                        |> List.filterMap convertLine
                        |> String.join "\n"
                   )

        Verbatim "align" -> "| equation" ++ "\n" ++ leftBodyString block.body

        Verbatim "math" ->
            "| math\n" ++ leftBodyString block.body

        Verbatim name ->
            let
                argsStr =
                    if List.isEmpty block.args then
                        ""
                    else
                        " " ++ String.join " " block.args
            in
            "| " ++ name ++ argsStr ++ "\n" ++ leftBodyString block.body

        Ordinary "itemize" ->
            case block.body of
                Right exprs ->
                    let
                        itemToString expr =
                            case expr of
                                Fun "item" args _ ->
                                    Just ("- " ++ (exprListToString args |> String.trim))

                                _ ->
                                    Nothing
                    in
                    "\n" ++ (List.filterMap itemToString exprs |> String.join "\n\n") ++ "\n"

                Left str ->
                    str

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

        VFun "math" str _ ->
            "[math " ++ str ++ "]"

        VFun "$$" str _ ->
            "$$\n" ++ str ++ "\n$$"

        VFun name str _ ->
            name ++ str

        Fun name args _ ->
            "[" ++ name ++ " " ++ exprListToString args ++ "]"

        ExprList _ exprs _ ->
            exprListToString exprs


parseNewcommand : String -> Maybe ( String, String )
parseNewcommand line =
    if String.startsWith "\\newcommand{\\" line then
        let
            afterPrefix =
                String.dropLeft 13 line

            -- find closing } of macro name
            nameEnd =
                String.indexes "}" afterPrefix |> List.head
        in
        case nameEnd of
            Just idx ->
                let
                    name =
                        String.left idx afterPrefix

                    rest =
                        String.dropLeft (idx + 1) afterPrefix

                    -- skip the [arity] part
                    afterArity =
                        case String.indexes "]" rest of
                            first :: _ ->
                                String.dropLeft (first + 1) rest

                            [] ->
                                rest

                    -- extract definition from {definition}
                    definition =
                        afterArity
                            |> String.trim
                            |> (\s ->
                                    if String.startsWith "{" s && String.endsWith "}" s then
                                        s |> String.dropLeft 1 |> String.dropRight 1
                                    else
                                        s
                               )
                in
                Just ( name, definition )

            Nothing ->
                Nothing

    else
        Nothing
