module Compiler.LaTeXData exposing (LaTeXData, initWithString, updateWithString)

{-| LaTeXData is the type that carries all the information needed for
efficient interactive editing. The `updateWithString` function is
optimized so as to do expensive computations (parsing) only on the
changed part of the text.

    Use `initWithString` to set up LaTeXData.  Once this is done,
    the field `renderedText` holds the rendered document.  Successive
    edits to it are made using `updateWithString`.  To render a document
    directly, use `renderDocument`.

@docs renderDocument, LaTeXData, initWithString, updateWithString

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


{-| -}
type alias LaTeXData =
    { lines : List String
    , blocks : List String
    , generations : List Int
    , parsedText : List (List Expression)
    , sourceMapIndex : List (List Int)
    , renderedText : List (Html LaTeXMsg)
    }


{-| Initialize LaTeXData using

    initWithString generation selectedId input

    - generation: an integer that changes on each edit;
      needed for optimization and proper rendering by
      virtual DOM

    - selectedId: a string which identifies an element
      in the rendered text that the user wants highlighted

    - input: the source text

-}
initWithString : Int -> String -> String -> LaTeXData
initWithString generation selectedId input =
    let
        state =
            Document.process generation input

        lines_ =
            String.lines input

        parsedText : List (List Expression)
        parsedText =
            Document.toParsed state
    in
    { lines = lines_
    , blocks = Document.toText state
    , generations = getGenerations parsedText
    , parsedText = parsedText
    , sourceMapIndex = Parser.Expression.sourceMapIndex (List.length lines_) parsedText
    , renderedText = render selectedId parsedText
    }


{-| For short documents or documents in a non-interactive editing context, use

    renderDocument generation selectedId input

otherwise, use initWithString, updateWithString, and LaTeXData

-}
renderDocument : Int -> String -> String -> List (Html LaTeXMsg)
renderDocument generation selectedId document =
    (initWithString generation selectedId document).renderedText


{-| `updateWithString` efficiently modifies the LaTeXState by identifying
the block of text that has changes, parsing and rendering that text,
and inserting the resulting parse data and rendered text in their
respective lists which are in turn fields of LaTeXState.

    updateWithString generation selectedId input data

The arguments are as with initWithString with one addition,
`data`, which is the current value of LaTeXData.

-}
updateWithString : Int -> String -> String -> LaTeXData -> LaTeXData
updateWithString generation selectedId input data =
    let
        oldBlocks : List String
        oldBlocks =
            data.blocks

        newBlocks : List String
        newBlocks =
            Block.compile generation input
                |> List.map (String.join "\n")

        blockDiffRecord =
            GenericDiffer.diff oldBlocks newBlocks

        prefixLength =
            List.length blockDiffRecord.commonInitialSegment

        deltNewBlocks : List String
        deltNewBlocks =
            blockDiffRecord.deltaInTarget

        input_ =
            String.lines input
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
            { lines = input_
            , blocks = newBlocks
            , generations = getGenerations parsedText
            , parsedText = parsedText
            , sourceMapIndex = Parser.Expression.sourceMapIndex (List.length input_) parsedText
            , renderedText = renderedTextBefore ++ deltaRenderedText ++ renderedTextAfter
            }


render : String -> List (List Expression) -> List (Html LaTeXMsg)
render selectedId parsed =
    parsed
        |> List.map (Render.render selectedId LaTeXState.init >> Html.div docStyle)


getGenerations : List (List Expression) -> List Int
getGenerations expressions =
    List.map (\e -> e |> List.head |> Maybe.map (Parser.Expression.getSource >> .generation) |> Maybe.withDefault 0) expressions


docStyle =
    [ HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5" ]
