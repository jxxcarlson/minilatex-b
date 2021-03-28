module Parser.Document exposing
    ( process, toParsed, toText
    , State, Block, BlockType(..), LineType(..)
    )

{-| The main function in this module is process, which takes as input
a string representing a MiniLaTeX document and produces as output
a value of type AST = List (List Expression).


## Functions

@docs process, toParsed, toText


## Types

@docs State, Block, BlockType, LineType

##@ About blocks

The block offset is the line number in the source text
at wwhich a block begins. Recall that a block is a string (which may contain newlines) derived
from a set of contiguous lines. It represents a logical paragraph: either an ordinary paragraph or
an outer begin-end pair. The offset describes the position of a string in the block. Thus, if
the text "a\\nb\\nc" starts at line 100 of the source text and is preceded and followed by a blank line,
then the block offset is 100, the offset of "a" is 0, the offest of "b" is 2, and the offest of "c" is 4.

-}

import Parser as P exposing ((|.), (|=))
import Parser.Expression exposing (Expression)
import Parser.Parser as Parser
import Parser.TextCursor exposing (TextCursor)
import Render.LaTeXState as LaTeXState exposing (LaTeXState)
import Render.Reduce as Reduce


{-| -}
type alias State =
    { input : List String
    , lineNumber : Int
    , generation : Int
    , blockType : BlockType
    , blockContents : List String
    , blockTypeStack : List BlockType
    , output : List TextCursor
    , laTeXState : LaTeXState
    }


{-| -}
type alias Block =
    { blockType : BlockType, content : List String }


{-| -}
type BlockType
    = Start
    | TextBlock
    | MathBlock
    | EnvBlock String
    | ErrorBlock


{-| -}
type LineType
    = LTBlank
    | LTTextBlock
    | LTMathBlock String
    | BeginEnvBlock String
    | EndEnvBlock String


{-| Compute the syntax tree and LaTeXState of a string of source text.
-}
process : Int -> List String -> State
process generation =
    runProcess generation



-- >> toParsed


{-| Compute the final State of a string of source text.
The output field of State holds the AST of the source text.

Function process operates a state machine which identifies logical
chunks of text, parses these using Parser.Parser.parseLoop,
and prepends them to a list of TextCursor.

-}
runProcess : Int -> List String -> State
runProcess generation strList =
    loop (init generation strList) nextState


{-| Return the AST from the State.
-}
toParsed : State -> List (List Expression)
toParsed state =
    state.output |> List.map .parsed


{-| Return a list of logical paragraphs (blocks(: ordinary paragraphs
or outer begin-end blocks.
-}
toText : State -> List String
toText state =
    state.output |> List.map .block


init : Int -> List String -> State
init generation strList =
    { input = strList
    , lineNumber = 0
    , generation = generation
    , blockType = Start
    , blockContents = []
    , blockTypeStack = []
    , output = []
    , laTeXState = LaTeXState.init
    }


nextState : State -> Step State State
nextState state_ =
    case List.head state_.input of
        Nothing ->
            Done (flush state_)

        Just currentLine ->
            let
                state =
                    { state_ | input = List.drop 1 state_.input }
            in
            case ( state.blockType, classify currentLine ) of
                ( Start, LTBlank ) ->
                    Loop (start state)

                ( Start, LTMathBlock _ ) ->
                    Loop (initBlock MathBlock currentLine state)

                ( Start, BeginEnvBlock blockType ) ->
                    Loop (pushBlockStack (EnvBlock blockType) currentLine state)

                ( Start, EndEnvBlock _ ) ->
                    Loop (initBlock ErrorBlock currentLine state)

                ( Start, LTTextBlock ) ->
                    Loop (initBlock TextBlock currentLine state)

                --
                ( ErrorBlock, LTBlank ) ->
                    Loop { state | blockType = Start, blockContents = [] }

                ( ErrorBlock, LTMathBlock _ ) ->
                    Loop (initWithBlockType MathBlock currentLine state)

                ( ErrorBlock, BeginEnvBlock blockType ) ->
                    Loop (initWithBlockType (EnvBlock blockType) currentLine state)

                ( ErrorBlock, EndEnvBlock _ ) ->
                    Loop (addToBlockContents ErrorBlock currentLine state)

                ( ErrorBlock, LTTextBlock ) ->
                    Loop (initWithBlockType TextBlock currentLine state)

                --
                ( TextBlock, LTBlank ) ->
                    -- Then end of a text block has been reached. Create a string representing
                    -- this block, parse it using Parser.parseLoop to produce a TextCursor, and
                    -- add it to state.output.  Finally, update the laTeXState using Render.Reduce.latexState
                    let
                        newTC : TextCursor
                        newTC =
                            Parser.parseLoop state.generation state.lineNumber (String.join "\n" (List.reverse state.blockContents))

                        output =
                            newTC :: state.output

                        laTeXState =
                            Reduce.laTeXState newTC.parsed state.laTeXState
                    in
                    Loop { state | blockType = Start, blockContents = [], laTeXState = laTeXState, output = output, lineNumber = state.lineNumber + countLines state.blockContents }

                ( TextBlock, LTMathBlock _ ) ->
                    Loop (initWithBlockType MathBlock currentLine state)

                ( TextBlock, BeginEnvBlock blockType ) ->
                    Loop (initWithBlockType (EnvBlock blockType) currentLine state)

                ( TextBlock, EndEnvBlock _ ) ->
                    Loop (addToBlockContents ErrorBlock currentLine state)

                ( TextBlock, LTTextBlock ) ->
                    Loop (addToBlockContents TextBlock currentLine state)

                --
                ( MathBlock, LTBlank ) ->
                    -- end of MathBlock reached. Update as above.
                    let
                        newTC =
                            Parser.parseLoop state.generation state.lineNumber (String.join "\n" (List.reverse state.blockContents))

                        output =
                            newTC :: state.output

                        laTeXState =
                            Reduce.laTeXState newTC.parsed state.laTeXState
                    in
                    Loop { state | blockType = Start, blockContents = [], laTeXState = laTeXState, output = output, lineNumber = state.lineNumber + countLines state.blockContents }

                ( MathBlock, LTMathBlock _ ) ->
                    Loop (initWithBlockType Start currentLine state)

                ( MathBlock, BeginEnvBlock blockType ) ->
                    Loop (initWithBlockType (EnvBlock blockType) currentLine state)

                ( MathBlock, EndEnvBlock _ ) ->
                    Loop (addToBlockContents ErrorBlock currentLine state)

                ( MathBlock, LTTextBlock ) ->
                    Loop (addToBlockContents MathBlock currentLine state)

                ---
                ( EnvBlock _, LTBlank ) ->
                    Loop state

                ( EnvBlock et, LTMathBlock _ ) ->
                    Loop (addToBlockContents (EnvBlock et) currentLine state)

                ( EnvBlock _, BeginEnvBlock blockType ) ->
                    Loop (pushBlockStack (EnvBlock blockType) currentLine state)

                ( EnvBlock et, EndEnvBlock _ ) ->
                    Loop (popBlockStack (EnvBlock et) currentLine state)

                ( EnvBlock et, LTTextBlock ) ->
                    Loop (addToBlockContents (EnvBlock et) currentLine state)



-- OPERATIONS ON STATE


{-| Put State in the Start state
-}
start : State -> State
start state =
    { state | blockType = Start, blockTypeStack = [], blockContents = [] }


initBlock : BlockType -> String -> State -> State
initBlock blockType_ currentLine_ state =
    { state | blockType = blockType_, blockContents = [ currentLine_ ] }


initWithBlockType : BlockType -> String -> State -> State
initWithBlockType blockType_ currentLine_ state =
    let
        newTC =
            Parser.parseLoop state.generation state.lineNumber (String.join "\n" (List.reverse (currentLine_ :: state.blockContents)))

        laTeXState =
            Reduce.laTeXState newTC.parsed state.laTeXState
    in
    { state
        | blockType = blockType_
        , blockContents = [ currentLine_ ]
        , lineNumber = state.lineNumber + countLines state.blockContents
        , laTeXState = laTeXState
        , output = newTC :: state.output
    }


addToBlockContents : BlockType -> String -> State -> State
addToBlockContents blockType_ currentLine_ state =
    { state | blockType = blockType_, blockContents = currentLine_ :: state.blockContents }


pushBlockStack : BlockType -> String -> State -> State
pushBlockStack blockType_ currentLine_ state =
    { state
        | blockType = blockType_
        , blockTypeStack = blockType_ :: state.blockTypeStack
        , blockContents = currentLine_ :: state.blockContents
    }


popBlockStack : BlockType -> String -> State -> State
popBlockStack blockType_ currentLine_ state =
    let
        newBlockTypeStack =
            List.drop 1 state.blockTypeStack
    in
    if newBlockTypeStack == [] then
        let
            input_ =
                String.join "\n" (List.reverse (currentLine_ :: state.blockContents))

            tc_ =
                Parser.parseLoop state.generation state.lineNumber input_

            tc =
                { tc_ | text = input_ }

            laTeXState =
                Reduce.laTeXState tc.parsed state.laTeXState
        in
        { state
            | blockType = Start
            , blockTypeStack = []
            , blockContents = currentLine_ :: state.blockContents
            , output = tc :: state.output
            , laTeXState = laTeXState
            , lineNumber = state.lineNumber + (2 + List.length state.blockContents) -- TODO: think about this.  Is it correct?
        }

    else
        { state
            | blockType = blockType_
            , blockTypeStack = newBlockTypeStack
            , blockContents = currentLine_ :: state.blockContents
        }


flush : State -> State
flush state =
    let
        input =
            String.join "\n" (List.reverse state.blockContents)

        tc_ =
            Parser.parseLoop state.generation state.lineNumber input

        tc =
            { tc_ | text = input }

        laTeXState =
            Reduce.laTeXState tc.parsed state.laTeXState
    in
    { state | laTeXState = laTeXState, output = List.reverse (tc :: state.output) }


countLines : List String -> Int
countLines list =
    list |> List.map String.lines |> List.concat |> List.length |> (\x -> x + 1)



-- HELPER (LOOP)


loop : State -> (State -> Step State State) -> State
loop s nextState_ =
    case nextState_ s of
        Loop s_ ->
            loop s_ nextState_

        Done b ->
            b


type Step state a
    = Loop state
    | Done a



-- CLASSIFY LINE


classify : String -> LineType
classify str =
    case P.run lineTypeParser str of
        Ok lt ->
            lt

        Err _ ->
            LTBlank


lineTypeParser =
    P.oneOf [ mathBlockParser, beginEnvLineParser, endEnvLineParser, textBlockParser, P.succeed LTBlank ]


beginEnvLineParser : P.Parser LineType
beginEnvLineParser =
    P.succeed (\s -> BeginEnvBlock s)
        |. P.symbol "\\begin{"
        |= P.getChompedString (P.chompUntil "}")


endEnvLineParser : P.Parser LineType
endEnvLineParser =
    P.succeed (\s -> EndEnvBlock s)
        |. P.symbol "\\end{"
        |= P.getChompedString (P.chompUntil "}")


mathBlockParser : P.Parser LineType
mathBlockParser =
    P.succeed LTMathBlock
        |= P.oneOf
            [ P.symbol "$$" |> P.map (\() -> "$$")
            , P.symbol "$" |> P.map (\() -> "$")
            , P.symbol "\\[" |> P.map (\() -> "\\[")
            , P.symbol "\\]" |> P.map (\() -> "\\]")
            , P.symbol "\\(" |> P.map (\() -> "\\(")
            , P.symbol "\\)" |> P.map (\() -> "\\)")
            ]


textBlockParser : P.Parser LineType
textBlockParser =
    P.succeed LTTextBlock
        |. P.chompIf (\_ -> True)
