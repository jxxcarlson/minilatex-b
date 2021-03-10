module MiniLaTeX exposing (compile, LaTeXData, initWithString, updateWithString)

{-| For simple applications, use

    MiniLaTeX.compile document

This function takes a string representing your MiniLaTeX
document as input and produces Html for your web app.
For more details, see the app `./simple-demo`.\`

For applications that use interactive editing, use _LaTeXData_
and the functions _initWithString_ and _updateWithString_
The function initWithString will set up a LaTeXData value.
It carries all the information needed for
efficient interactive editing. Once this is done,
the field _renderedText_ holds the rendered document. Successive
edits to it are made using updateWithString.

@docs compile, LaTeXData, initWithString, updateWithString

-}

import Compiler.Differ as Differ
import Compiler.GenericDiffer as GenericDiffer
import Html exposing (Html)
import Html.Attributes as HA
import LaTeXMsg exposing (LaTeXMsg(..))
import Parser.Block as Block
import Parser.Document as Document
import Parser.Expression exposing (Expression)
import Parser.TextCursor
import Render.Accumulator as Accumulator
import Render.LaTeXState as LaTeXState exposing (LaTeXState)
import Render.Render as Render


{-| -}
type alias LaTeXData =
    { lines : List String
    , blocks : List String
    , generations : List Int
    , parsedText : List (List Expression)
    , sourceMapIndex : List (List Int)
    , renderedText : List (Html LaTeXMsg)
    , laTeXState : LaTeXState
    }


{-| Initialize LaTeXData using

initWithString generation selectedId input

  - _generation_: an integer that changes on each edit;
    needed for optimization and proper rendering by
    virtual DOM

  - _selectedId_: a string which identifies an element
    in the rendered text that the user wants highlighted

  - _input_ the source text

-}
initWithString2 : Int -> String -> String -> LaTeXData
initWithString2 generation selectedId input =
    let
        state : Document.State
        state =
            Document.process generation input

        lines_ =
            String.lines input

        parsedText : List (List Expression)
        parsedText =
            Document.toParsed state

        accumulatorState =
            Accumulator.render selectedId state.laTeXState parsedText
    in
    { lines = lines_
    , blocks = Document.toText state
    , generations = getGenerations parsedText
    , parsedText = parsedText
    , sourceMapIndex = Parser.Expression.sourceMapIndex (List.length lines_) parsedText
    , renderedText = accumulatorState.html
    , laTeXState = accumulatorState.state
    }


initWithString : Int -> String -> String -> LaTeXData
initWithString generation selectedId input =
    let
        state : Document.State
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
    , renderedText = render selectedId state.laTeXState parsedText
    , laTeXState = state.laTeXState
    }


{-| Use `compile` for short documents or documents in a non-interactive editing context.

    compile document

Otherwise, use initWithString, updateWithString, and LaTeXData

-}
compile : String -> List (Html LaTeXMsg)
compile document =
    (initWithString 0 "nada" document).renderedText


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
                    render selectedId data.laTeXState deltaParsed

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
            , laTeXState = data.laTeXState -- TODO: this is very crude: make it better!
            }


render : String -> LaTeXState -> List (List Expression) -> List (Html LaTeXMsg)
render selectedId laTeXSTate parsed =
    parsed
        |> List.map (Render.render selectedId laTeXSTate >> Html.div docStyle)


getGenerations : List (List Expression) -> List Int
getGenerations expressions =
    List.map (\e -> e |> List.head |> Maybe.map (Parser.Expression.getSource >> .generation) |> Maybe.withDefault 0) expressions


docStyle =
    [ HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5" ]
