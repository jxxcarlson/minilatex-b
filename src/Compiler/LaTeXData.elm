module Compiler.LaTeXData exposing (..)

import Compiler.Differ as Differ
import Html exposing (Html)
import Html.Attributes as HA
import Parser.Document as Document
import Parser.Expression exposing (Expression)
import Render.LaTeXState as LaTeXState exposing (LaTeXMsg)
import Render.Render as Render


type alias LaTeXData =
    { lines : List String
    , blocks : List String
    , parsedText : List (List Expression)
    , sourceMapIndex : List (List Int)
    , renderedText : Html LaTeXMsg
    }


initWithString : Int -> String -> LaTeXData
initWithString generation input_ =
    let
        state =
            Document.process generation input_

        lines_ =
            String.lines input_

        parsedText =
            Document.toParsed state
    in
    { lines = lines_
    , blocks = Document.toText state
    , parsedText = parsedText
    , sourceMapIndex = Parser.Expression.sourceMapIndex (List.length lines_) parsedText
    , renderedText = render "nada" parsedText
    }


updateWithString : Int -> String -> String -> LaTeXData -> LaTeXData
updateWithString generation elementId input_ data =
    let
        state =
            Document.process generation input_

        input =
            String.lines input_

        dr =
            Differ.diff (input_ |> String.lines)
                data.lines

        parsedText =
            Document.toParsed state
    in
    { lines = input
    , blocks = Document.toText state
    , parsedText = parsedText
    , sourceMapIndex = Parser.Expression.sourceMapIndex (List.length input) parsedText
    , renderedText = render elementId parsedText
    }


render : String -> List (List Expression) -> Html LaTeXMsg
render id parsed =
    parsed
        |> List.map (Render.render id LaTeXState.init >> Html.div docStyle)
        |> Html.div []


docStyle =
    [ HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5" ]
