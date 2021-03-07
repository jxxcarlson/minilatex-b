module Compiler.LaTeXData exposing (..)

import Compiler.Differ as Differ
import Html exposing (Html)
import Html.Attributes as HA
import Parser.Document as Document
import Parser.Expression exposing (Expression)
import Parser.TextCursor
import Render.LaTeXState as LaTeXState exposing (LaTeXMsg)
import Render.Render as Render


type alias LaTeXData =
    { lines : List String
    , blocks : List String
    , parsedText : List (List Expression)
    , sourceMapIndex : List (List Int)
    , renderedText : Html LaTeXMsg
    }


initWithString : Int -> String -> String -> LaTeXData
initWithString generation id input_ =
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

    --, renderedText = render (String.fromInt generation ++ ":" ++ id) parsedText
    , renderedText = render id parsedText
    }


updateWithString : Int -> String -> String -> LaTeXData -> LaTeXData
updateWithString generation elementId input_ data =
    let
        state =
            Document.process generation input_

        input =
            String.lines input_

        dr =
            Differ.diff data.lines (input_ |> String.lines)

        lineNumber =
            (Differ.range dr).firstChange

        bi =
            Differ.getBlockIndex (Debug.log "LINE NO" lineNumber) (Debug.log "SM IIND" data.sourceMapIndex)
                -- TODO: Dangerous?
                |> Maybe.withDefault 0
                |> Debug.log "BI"

        _ =
            Debug.log "DELTA S" dr.deltaInSource

        _ =
            Debug.log "DELTA T" dr.deltaInTarget

        deltaState =
            Document.process generation (String.join "\n" dr.deltaInTarget)
                |> (\state_ -> { state_ | output = List.map (Parser.TextCursor.incrementBlockOffset lineNumber) state_.output })
                |> Debug.log "DELTA STATE"

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
