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
    , generations : List Int
    , parsedText : List (List Expression)
    , sourceMapIndex : List (List Int)
    , renderedText : List (Html LaTeXMsg)
    }


initWithString : Int -> String -> String -> LaTeXData
initWithString generation id input_ =
    let
        state =
            Document.process generation input_

        lines_ =
            String.lines input_

        parsedText : List (List Expression)
        parsedText =
            Document.toParsed state
    in
    { lines = lines_
    , blocks = Document.toText state
    , generations = getGenerations parsedText
    , parsedText = parsedText
    , sourceMapIndex = Parser.Expression.sourceMapIndex (List.length lines_) parsedText
    , renderedText = render id parsedText
    }


getGenerations : List (List Expression) -> List Int
getGenerations expressions =
    List.map (\e -> e |> List.head |> Maybe.map (Parser.Expression.getSource >> .generation) |> Maybe.withDefault 0) expressions


updateWithString : Int -> String -> String -> LaTeXData -> LaTeXData
updateWithString generation selectedId input_ data =
    let
        state : Document.State
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

        _ =
            Debug.log "BLOCKS AFTER" <| Differ.blockAfter_ (bi + 1) (List.map String.lines data.blocks)

        _ =
            Debug.log "BLOCKS (NEW)" <| Document.toText state

        incrementTextCursor =
            Parser.TextCursor.incrementBlockOffset lineNumber >> Parser.TextCursor.incrementBlockIndex bi

        deltaOutput : List (List Expression)
        deltaOutput =
            Document.process generation (String.join "\n" dr.deltaInTarget)
                |> (\state_ -> { state_ | output = List.map incrementTextCursor state_.output })
                |> Document.toParsed
                |> Debug.log "DELTA STATE"

        parsedText =
            Document.toParsed state
    in
    { lines = input
    , blocks = Document.toText state
    , generations = getGenerations parsedText
    , parsedText = parsedText
    , sourceMapIndex = Parser.Expression.sourceMapIndex (List.length input) parsedText
    , renderedText = render selectedId parsedText
    }


render : String -> List (List Expression) -> List (Html LaTeXMsg)
render selectedId parsed =
    parsed
        |> List.map (Render.render selectedId LaTeXState.init >> Html.div docStyle)


docStyle =
    [ HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5" ]
