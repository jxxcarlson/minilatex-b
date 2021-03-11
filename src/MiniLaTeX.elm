module MiniLaTeX exposing
    ( compile
    , LaTeXData, initWithString, updateWithString
    , viewLaTeXData, viewLaTeXDataAsElement
    )

{-| For simple applications, use

    MiniLaTeX.compile document

This function takes a string representing your MiniLaTeX
document as input and produces Html for your web app.
For more details, see the app `./simple-demo`.\`

For applications that use interactive editing, use _LaTeXData_
and the functions _initWithString_ and _updateWithString_.

The function initWithString will set up a LaTeXData value.
It carries all the information needed for
efficient interactive editing. Once this is done,
the field _renderedText_ holds the rendered document. Successive
edits to it are made using updateWithString.


## Render

@docs compile, renderLaTeXDataToHtml, renderLaTeXDataToElement


## LaTeXData

@docs LaTeXData, initWithString, updateWithString

-}

import Compiler.Differ as Differ
import Compiler.GenericDiffer as GenericDiffer
import Element exposing (Element)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import LaTeXMsg exposing (LaTeXMsg(..))
import Parser.Block as Block
import Parser.Document as Document
import Parser.Expression exposing (Expression)
import Parser.TextCursor
import Render.Accumulator as Accumulator
import Render.LaTeXState exposing (LaTeXState)
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

        accumulatorState =
            Accumulator.render selectedId state.laTeXState parsedText

        rt : List (Html LaTeXMsg)
        rt =
            accumulatorState.html |> List.reverse |> List.map (\x -> Html.span docStyle x)
    in
    { lines = lines_
    , blocks = Document.toText state
    , generations = getGenerations parsedText
    , parsedText = parsedText
    , sourceMapIndex = Parser.Expression.sourceMapIndex (List.length lines_) parsedText
    , renderedText = rt
    , laTeXState = accumulatorState.state
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
            -- TODO: this is a crude way to handle non-localized edits; let's make it better
            initWithString generation selectedId input

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

                accumulatorState =
                    Accumulator.render selectedId data.laTeXState deltaParsed

                deltaRenderedText : List (Html LaTeXMsg)
                deltaRenderedText =
                    accumulatorState.html |> List.reverse |> List.map (\x -> Html.span docStyle x)

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


{-| Render a LaTeXData value to `Html LaTeXMsg` given style information.
-}
viewLaTeXData : List (Html.Attribute LaTeXMsg) -> LaTeXData -> Html LaTeXMsg
viewLaTeXData style laTeXData =
    Html.div style
        (List.map2 mathNode laTeXData.generations laTeXData.renderedText)


{-| Similar to `renderLaTeXDataToHtml`: render a LaTeXData value to `Element LaTeXMsg` given style information.
See `./app/Main.elm` for an example
-}
viewLaTeXDataAsElement : List (Html.Attribute LaTeXMsg) -> LaTeXData -> Element LaTeXMsg
viewLaTeXDataAsElement style laTeXData =
    viewLaTeXData style laTeXData |> Element.html


mathNode : Int -> Html LaTeXMsg -> Html LaTeXMsg
mathNode generation html =
    Html.Keyed.node "div" laTeXStyle [ ( String.fromInt generation, html ) ]


laTeXStyle =
    [ HA.style "margin-bottom" "12px" ]



-- HELPERS


getGenerations : List (List Expression) -> List Int
getGenerations expressions =
    List.map (\e -> e |> List.head |> Maybe.map (Parser.Expression.getSource >> .generation) |> Maybe.withDefault 0) expressions


docStyle =
    [ HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5" ]
