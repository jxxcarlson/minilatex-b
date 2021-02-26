module Render exposing (..)

import Config
import Html exposing (Html)
import Html.Attributes as HA
import Json.Encode
import Parser.Expression exposing (Expression(..))
import Parser.Parser as PP


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


render : List Expression -> List (Html msg)
render exprs =
    List.map renderExpr exprs


renderExpr : Expression -> Html msg
renderExpr expr =
    case expr of
        Text s sm ->
            Html.span Config.textSpanStyle [ Html.text s ]

        InlineMath s sm ->
            inlineMathText s

        DisplayMath s sm ->
            displayMathText s

        Macro name optArg args sm ->
            macro name optArg args

        LXList list_ ->
            List.map renderExpr list_ |> Html.span Config.textSpanStyle

        LXError s p sm ->
            Html.span []
                [ Html.span Config.errorStyle2 [ Html.text s ]
                , Html.span Config.errorStyle [ Html.text (errorString p sm) ]
                ]


errorString : Parser.Expression.Problem -> Parser.Expression.SourceMap -> String
errorString p sm =
    " << "
        ++ Parser.Expression.problemAsString p
        ++ " // Pos ("
        ++ String.fromInt sm.lineNumber
        ++ ", "
        ++ String.fromInt sm.offset
        ++ ")"


macro : String -> Maybe String -> List String -> Html msg
macro name optArg args =
    Html.text
        ("\\"
            ++ name
            ++ "["
            ++ Maybe.withDefault "null" optArg
            ++ "]"
            ++ (args |> List.map (\x -> "{" ++ x ++ "}") |> String.join "")
        )


mathText : DisplayMode -> String -> Html msg
mathText displayMode content =
    Html.node "math-text"
        [ HA.property "delay" (Json.Encode.bool False)
        , HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
        , HA.property "content" (Json.Encode.string (content |> String.replace "\\ \\" "\\\\"))

        --, HA.property "content" (Json.Encode.string content |> String.replace "\\ \\" "\\\\"))
        ]
        []


isDisplayMathMode : DisplayMode -> Bool
isDisplayMathMode displayMode =
    case displayMode of
        InlineMathMode ->
            False

        DisplayMathMode ->
            True


inlineMathText : String -> Html msg
inlineMathText str_ =
    mathText InlineMathMode (String.trim str_)


displayMathText : String -> Html msg
displayMathText str_ =
    mathText DisplayMathMode (String.trim str_)


highLight : String -> List (List Expression) -> List (Html msg)
highLight str expressionList =
    let
        lines : List String
        lines =
            String.lines str

        sourceMaps : List Parser.Expression.SourceMap
        sourceMaps =
            PP.getErrors expressionList |> List.map Parser.Expression.getSource
    in
    List.map2 highlightWithSourceMap sourceMaps lines


highlightWithSourceMap : Parser.Expression.SourceMap -> String -> Html msg
highlightWithSourceMap sm str =
    let
        slice =
            Parser.Expression.sliceWithSourceMap sm str
    in
    Html.span []
        [ Html.span [] [ Html.text slice.left ]
        , Html.span [ HA.style "background-color" "pink" ] [ Html.text slice.middle ]
        , Html.span [] [ Html.text slice.right ]
        ]
