module Render.Render exposing (..)

import Config
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Json.Encode
import List.Extra
import Parser.Expression exposing (Expression(..), SourceMap)
import Parser.Parser as PP


type LaTeXMsg
    = SendSourceMap SourceMap


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


render : List Expression -> List (Html LaTeXMsg)
render exprs =
    List.map renderExpr exprs


clicker sm =
    onClick (SendSourceMap sm)


renderExpr : Expression -> Html LaTeXMsg
renderExpr expr =
    case expr of
        Text s sm ->
            Html.span (clicker sm :: Config.textSpanStyle) [ Html.text s ]

        InlineMath s sm ->
            inlineMathText s sm

        DisplayMath s sm ->
            displayMathText s sm

        Macro name optArg args sm ->
            macro name optArg args sm

        LXList list_ ->
            List.map renderExpr list_ |> Html.span Config.textSpanStyle

        LXError s p sm ->
            Html.span []
                [ Html.span (clicker sm :: Config.errorStyle2) [ Html.text s ]
                , Html.span (clicker sm :: Config.errorStyle) [ Html.text (errorString p sm) ]
                ]

        LXNull () _ ->
            Html.span [] [ Html.text " " ]


errorString : Parser.Expression.Problem -> Parser.Expression.SourceMap -> String
errorString p sm =
    " << "
        ++ Parser.Expression.problemAsString p


macro : String -> Maybe String -> List Expression -> SourceMap -> Html LaTeXMsg
macro name optArg args sm =
    case Dict.get name macroDict of
        Nothing ->
            undefinedMacro name sm

        Just f ->
            f optArg args sm


undefinedMacro : String -> SourceMap -> Html LaTeXMsg
undefinedMacro name sm =
    Html.span [ clicker sm, HA.style "color" "red" ] [ Html.text "Undefined macro: " ]


mathText : DisplayMode -> String -> SourceMap -> Html LaTeXMsg
mathText displayMode content sm =
    Html.node "math-text"
        [ HA.property "delay" (Json.Encode.bool False)
        , HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
        , HA.property "content" (Json.Encode.string (content |> String.replace "\\ \\" "\\\\"))
        , clicker sm

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


inlineMathText : String -> SourceMap -> Html LaTeXMsg
inlineMathText str_ sm =
    mathText InlineMathMode (String.trim str_) sm


displayMathText : String -> SourceMap -> Html LaTeXMsg
displayMathText str_ sm =
    mathText DisplayMathMode (String.trim str_) sm


at : Int -> String -> Maybe String
at k str =
    String.lines str |> List.Extra.getAt k


highLight : String -> List (List Expression) -> List (Html msg)
highLight str expressionList =
    let
        lines : List String
        lines =
            String.lines str

        sourceMaps : List Parser.Expression.SourceMap
        sourceMaps =
            PP.getErrors expressionList |> List.map Parser.Expression.getSource

        _ =
            Debug.log "(lines, sourceMaps)" ( List.length lines, List.length sourceMaps )
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



-- MACRO DICT


type alias MacroDict msg =
    Dict String (Maybe String -> List Expression -> SourceMap -> Html msg)


macroDict : MacroDict LaTeXMsg
macroDict =
    Dict.fromList
        [ ( "strong", \ms args sm -> render args |> Html.span [ clicker sm, HA.style "font-weight" "bold" ] )
        , ( "italic", \ms args sm -> render args |> Html.span [ clicker sm, HA.style "font-style" "italic" ] )
        , ( "foo", \ms args sm -> Html.span [ clicker sm, HA.style "font-style" "italic" ] [ Html.text "Foo" ] )
        ]
