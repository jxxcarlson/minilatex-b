module Render.Render exposing (..)

import Config
import Dict exposing (Dict)
import Element exposing (Element)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Json.Encode
import List.Extra
import Parser.Expression exposing (Expression(..), SourceMap)
import Render.LaTeXState as LaTexState exposing (LaTeXMsg(..), LaTeXState)


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


render : LaTeXState -> List Expression -> List (Html LaTeXMsg)
render state exprs =
    List.map (renderExpr state) exprs


clicker sm =
    onClick (SendSourceMap sm)


renderExpr : LaTeXState -> Expression -> Html LaTeXMsg
renderExpr state expr =
    case expr of
        Text s sm ->
            Html.span (clicker sm :: state.config.textSpanStyle) [ Html.text s ]

        InlineMath s sm ->
            inlineMathText s sm

        DisplayMath s sm ->
            displayMathText s sm

        Macro name optArg args sm ->
            macro state name optArg args sm

        Environment name args body sm ->
            environment state name args body sm

        LXList list_ ->
            List.map (renderExpr state) list_ |> Html.span Config.textSpanStyle

        LXError s p sm ->
            Html.span [ clicker (Debug.log "SM" { sm | offset = sm.offset - sm.length }) ]
                [ Html.span Config.errorStyle2 [ Html.text s ]
                , Html.span Config.errorStyle [ Html.text (errorString p sm) ]
                ]

        LXNull () _ ->
            Html.span [] [ Html.text " " ]


errorString : Parser.Expression.Problem -> Parser.Expression.SourceMap -> String
errorString p sm =
    " << "
        ++ Parser.Expression.problemAsString p


macro : LaTeXState -> String -> Maybe String -> List Expression -> SourceMap -> Html LaTeXMsg
macro state name optArg args sm =
    case Dict.get name macroDict of
        Nothing ->
            undefinedMacro name sm

        Just f ->
            f state optArg args sm


undefinedMacro : String -> SourceMap -> Html LaTeXMsg
undefinedMacro name sm =
    Html.span [ clicker sm, HA.style "color" "red" ] [ Html.text "Undefined macro: " ]


environment state name args body sm =
    -- TODO: Implement environment renderer
    Html.span [] [ Html.text <| "Environment: " ++ name ++ " ... not implemented" ]



-- END: RENDER ENVIO


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


highlightWithSourceMap : Parser.Expression.SourceMap -> String -> List (List Int) -> Html msg
highlightWithSourceMap sm str sourceMapIndex_ =
    let
        selection =
            Parser.Expression.getSelectionFromSourceMap sm str sourceMapIndex_

        idxs =
            String.indices selection str
    in
    case List.head idxs of
        Nothing ->
            Html.span [] [ Html.text str ]

        Just offset ->
            let
                left =
                    String.left offset str

                remainder =
                    String.dropLeft offset str

                middle =
                    String.left sm.length remainder

                right =
                    String.dropLeft sm.length remainder
            in
            Html.div []
                [ Html.span [] [ Html.text left ]
                , Html.span [ HA.style "background-color" "pink" ] [ Html.text middle ]
                , Html.span [] [ Html.text right ]
                ]



-- MACRO DICT


type alias MacroDict =
    Dict String (LaTeXState -> Maybe String -> List Expression -> SourceMap -> Html LaTeXMsg)



--macroDict : MacroDict


macroDict =
    Dict.fromList
        [ ( "strong", \state ms args sm -> render state args |> Html.span [ clicker sm, HA.style "font-weight" "bold" ] )
        , ( "italic", \state ms args sm -> render state args |> Html.span [ clicker sm, HA.style "font-style" "italic" ] )
        , ( "red", \state ms args sm -> render state args |> Html.span [ clicker sm, HA.style "color" Config.redColor ] )
        , ( "blue", \state ms args sm -> render state args |> Html.span [ clicker sm, HA.style "color" Config.blueColor ] )
        , ( "foo", \state ms args sm -> Html.span [ clicker sm, HA.style "font-style" "italic" ] [ Html.text "Foo" ] )
        ]
