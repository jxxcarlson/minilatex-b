module MiniLaTeX exposing
    ( LaTeXData
    , compile, init, update
    , compileFromString, initWithString, updateWithString
    , viewLaTeXData, viewLaTeXDataAsElement
    )

{-| The functions _init_ and _update_ produce a value of type
_LaTeXData_. Use _viewLaTeXData_ or _viewLaTeXDataAsElement_ to
view this data structure in your app.


## Types

@docs LaTeXData


## API for document as a list of strings

@docs compile, init, update


## API for document as a string

@docs compileFromString, initWithString, updateWithString


## View

@docs viewLaTeXData, viewLaTeXDataAsElement

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

    init generation selectedId input

  - _generation_: an integer that changes on each edit;
    needed for optimization and proper rendering by
    virtual DOM. Your app must provide this number
    and increment it as needed.

  - _selectedId_: a string which identifies an element
    in the rendered text that the user wants highlighted

  - _input_: the source text as a list of lines

-}
init : Int -> String -> List String -> LaTeXData
init generation selectedId input =
    let
        state : Document.State
        state =
            Document.process generation input

        lines_ =
            input

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


{-| Like `init`, but takes a string as input for the source text
-}
initWithString : Int -> String -> String -> LaTeXData
initWithString generation selectedId input =
    init generation selectedId (String.lines input)


{-| -}
compile : List String -> List (Html LaTeXMsg)
compile document =
    (init 0 "nada" document).renderedText


{-| Like `compile`, but takes a string as input.
-}
compileFromString : String -> List (Html LaTeXMsg)
compileFromString document =
    (initWithString 0 "nada" document).renderedText


{-| This function efficiently modifies the LaTeXState by identifying
the block of text that has changed, parsing and rendering that text,
then inserting the resulting parse data and rendered text in their
respective lists which are in turn fields of LaTeXState.

    update generation selectedId input data

The arguments are as with init with one addition,
`data`, which is the current LaTeXData value.

-}
update : Int -> String -> List String -> LaTeXData -> LaTeXData
update generation selectedId input data =
    let
        -- (1) COMPUTE NEW BLOCKS
        oldBlocks : List String
        oldBlocks =
            data.blocks

        newBlocks : List String
        newBlocks =
            Block.compile generation input
                |> List.map (String.join "\n")

        -- (2) COMPUTE DIFF OF BLOCKS
        blockDiffRecord : GenericDiffer.DiffRecord String
        blockDiffRecord =
            GenericDiffer.diff oldBlocks newBlocks

        deltaNewBlocks : List String
        deltaNewBlocks =
            blockDiffRecord.deltaInTarget

        -- (3) COMPUTE DIFF OF PARSED TEXT
        prefixLength =
            List.length blockDiffRecord.commonInitialSegment

        deltaSourceLength =
            List.length blockDiffRecord.deltaInSource

        parsedBefore =
            Differ.blocksBefore_ prefixLength data.parsedText

        parsedBetween =
            Differ.slice prefixLength (prefixLength + 1) data.parsedText

        parsedAfter =
            Differ.blockAfter_ (prefixLength + deltaSourceLength) data.parsedText

        mSourceMap =
            Maybe.map Parser.Expression.getSource (List.head parsedBetween |> Maybe.andThen List.head)

        blockOffset_ =
            Maybe.map .blockOffset mSourceMap

        incrementTextCursor =
            Parser.TextCursor.incrementBlockOffset (blockOffset_ |> Maybe.withDefault 0)

        deltaParsed : List (List Expression)
        deltaParsed =
            if deltaNewBlocks == [] then
                []

            else
                Document.process generation (List.reverse deltaNewBlocks)
                    |> (\state_ -> { state_ | output = List.map incrementTextCursor state_.output })
                    |> Document.toParsed

        parsedText =
            parsedBefore ++ deltaParsed ++ parsedAfter

        -- (4) COMPUTE DIFF OF RENDERED TEXT
        renderedTextBefore : List (Html LaTeXMsg)
        renderedTextBefore =
            List.take prefixLength data.renderedText

        accumulatorState : Accumulator.ReducerData
        accumulatorState =
            Accumulator.render selectedId data.laTeXState deltaParsed

        deltaRenderedText : List (Html LaTeXMsg)
        deltaRenderedText =
            accumulatorState.html |> List.reverse |> List.map (\x -> Html.span docStyle x)

        renderedTextAfter : List (Html LaTeXMsg)
        renderedTextAfter =
            List.drop (prefixLength + deltaSourceLength) data.renderedText

        -- TODO: USE THE SIMPLIFICATION BELOW FOR NOW.  Then all errors must come from
        -- TODO (1) compiling the blocks (2) diffing, or (3) differential parsing
        renderedText =
            Accumulator.render selectedId data.laTeXState parsedText |> .html |> List.reverse |> List.map (\x -> Html.span docStyle x)

        -- (5) RECORD UPDATED LATEX DATA
    in
    { lines = input
    , blocks = newBlocks
    , generations = getGenerations parsedText
    , parsedText = parsedText
    , sourceMapIndex = Parser.Expression.sourceMapIndex (List.length input) parsedText
    , renderedText = renderedText -- renderedTextBefore ++ deltaRenderedText ++ renderedTextAfter
    , laTeXState = data.laTeXState -- TODO: this is very crude: make it better!
    }


{-| Like update, but takes a string as input.
-}
updateWithString : Int -> String -> String -> LaTeXData -> LaTeXData
updateWithString generation selectedId input data =
    update generation selectedId (String.lines input) data


render : String -> LaTeXState -> List (List Expression) -> List (Html LaTeXMsg)
render selectedId laTeXSTate parsed =
    parsed
        |> List.map (Render.render selectedId laTeXSTate >> Html.div docStyle)


{-| Render a LaTeXData value to `Html LaTeXMsg` given style information.
-}
viewLaTeXData : LaTeXData -> Html LaTeXMsg
viewLaTeXData laTeXData =
    Html.div []
        (List.map2 mathNode laTeXData.generations laTeXData.renderedText)


{-| Similar to `renderLaTeXDataToHtml`: render a LaTeXData value to `Element LaTeXMsg` given style information.
See `./app/Main.elm` for an example
-}
viewLaTeXDataAsElement : LaTeXData -> Element LaTeXMsg
viewLaTeXDataAsElement laTeXData =
    viewLaTeXData laTeXData |> Element.html


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
