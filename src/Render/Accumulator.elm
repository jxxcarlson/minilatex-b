module Render.Accumulator exposing (ReducerData, render)

{-| Accumulator.render renders parser data using LaTeXState/
Each time it consumes a

    block : List Expression

it updates LaTeXExpression, then uses that updated value to render the
block. Used in module MiniLaTeX.

@docs ReducerData, render

-}

import Html exposing (Html)
import Html.Attributes as HA
import LaTeXMsg exposing (LaTeXMsg(..))
import Parser.Expression exposing (Expression)
import Render.LaTeXState as LaTeXState exposing (LaTeXState)
import Render.Reduce as Reduce
import Render.Render as Render


{-| -}
type alias ReducerData =
    { state : LaTeXState, html : List (Html LaTeXMsg) }


basicRender : String -> LaTeXState -> List (List Expression) -> List (Html LaTeXMsg)
basicRender selectedId laTeXSTate parsed =
    parsed
        |> List.map (Render.render selectedId laTeXSTate >> Html.div docStyle)


{-| -}
render : String -> LaTeXState -> List (List Expression) -> ReducerData
render selectedId laTeXSTate expressionList =
    let
        initialData =
            { state = LaTeXState.reset laTeXSTate, html = [] }
    in
    List.foldl (reducer selectedId) initialData expressionList


reducer : String -> List Expression -> ReducerData -> ReducerData
reducer selectedId expressionList reducerData =
    let
        newLaTeXState =
            Reduce.laTeXState expressionList reducerData.state

        newHtml : List (Html LaTeXMsg)
        newHtml =
            Render.render selectedId newLaTeXState expressionList
    in
    { state = newLaTeXState, html = reducerData.html ++ newHtml }


docStyle =
    [ HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5" ]
