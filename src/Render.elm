module Render exposing (..)

import Parser.Expression exposing(Expression(..))
import Html exposing(Html)
import Html.Attributes as HA
import Config
import Json.Encode

type DisplayMode
    = InlineMathMode
    | DisplayMathMode

render : List Expression -> List (Html msg)
render exprs =
    List.map renderExpr exprs

renderExpr : Expression -> Html msg
renderExpr expr =
    case expr of
        Text s sm -> Html.span Config.textSpanStyle [Html.text s]
        InlineMath s sm -> inlineMathText s
        DisplayMath s sm -> displayMathText s
        LXList list_ -> List.map renderExpr list_ |> Html.span Config.textSpanStyle
        LXError s p sm -> Html.div Config.errorStyle
          [Html.text ("Error: (" ++ Parser.Expression.problemAsString p ++ ") " ++ s)]



mathText : DisplayMode -> String -> Html msg
mathText displayMode content =
    Html.node "math-text"
        [ HA.property "delay" (Json.Encode.bool False)
        , HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
        , HA.property "content" (Json.Encode.string (content  |> String.replace "\\ \\" "\\\\"))
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
inlineMathText  str_ =
    mathText InlineMathMode (String.trim str_)


displayMathText : String -> Html msg
displayMathText str_ =
    mathText DisplayMathMode (String.trim str_)

