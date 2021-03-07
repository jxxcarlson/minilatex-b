module Compiler.LaTeXData exposing (LaTeXData, initWithString, updateWithString)

{-| LaTeXData is a data structure which holds all the information needed to
interactively edit, rendering in real time, a MiniLaTeX document:

    type alias LaTeXData =
        { lines : List String
        , blocks : List String
        , generations : List Int
        , parsedText : List (List Expression)
        , sourceMapIndex : List (List Int)
        , renderedText : List (Html LaTeXMsg)
        }

@docs LaTeXData, initWithString, updateWithString

-}

import Compiler.Differ as Differ
import Compiler.GenericDiffer as GenericDiffer
import Html exposing (Html)
import Html.Attributes as HA
import Parser.Block as Block
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
        oldBlocks : List String
        oldBlocks =
            data.blocks

        newBlocks : List String
        newBlocks =
            Block.compile generation input_
                |> List.map (String.join "\n")

        blockDiffRecord =
            GenericDiffer.diff oldBlocks newBlocks

        prefixLength =
            List.length blockDiffRecord.commonInitialSegment

        deltNewBlocks : List String
        deltNewBlocks =
            blockDiffRecord.deltaInTarget

        input =
            String.lines input_
    in
    case deltNewBlocks |> List.head of
        Nothing ->
            data

        Just changedBlock ->
            let
                parsedBefore =
                    Differ.blocksBefore_ prefixLength data.parsedText

                parsedBetween =
                    Differ.slice prefixLength (prefixLength + 1) data.parsedText

                parsedAfter =
                    Differ.blockAfter_ (prefixLength + 1) data.parsedText

                mSourceMap =
                    Maybe.map Parser.Expression.getSource (List.head parsedBetween |> Maybe.andThen List.head)

                blockOffset_ =
                    Maybe.map .blockOffset mSourceMap

                incrementTextCursor =
                    Parser.TextCursor.incrementBlockOffset (blockOffset_ |> Maybe.withDefault 0)

                deltaParsed : List (List Expression)
                deltaParsed =
                    Document.process generation changedBlock
                        |> (\state_ -> { state_ | output = List.map incrementTextCursor state_.output })
                        |> Document.toParsed

                parsedText =
                    parsedBefore ++ deltaParsed ++ parsedAfter

                renderedTextBefore : List (Html LaTeXMsg)
                renderedTextBefore =
                    List.take prefixLength data.renderedText

                deltaRenderedText : List (Html LaTeXMsg)
                deltaRenderedText =
                    render selectedId deltaParsed

                renderedTextAfter : List (Html LaTeXMsg)
                renderedTextAfter =
                    List.drop (prefixLength + 1) data.renderedText
            in
            { lines = input
            , blocks = newBlocks
            , generations = getGenerations parsedText
            , parsedText = parsedText
            , sourceMapIndex = Parser.Expression.sourceMapIndex (List.length input) parsedText
            , renderedText = renderedTextBefore ++ deltaRenderedText ++ renderedTextAfter
            }


render : String -> List (List Expression) -> List (Html LaTeXMsg)
render selectedId parsed =
    parsed
        |> List.map (Render.render selectedId LaTeXState.init >> Html.div docStyle)


docStyle =
    [ HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5" ]
