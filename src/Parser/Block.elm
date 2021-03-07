module Parser.Block exposing (compile)

{-| The main function in this module is process, which takes as input
a string representing a MiniLaTeX document and produces as output
a value of type AST = List (List Expression).

@docs process

-}

import Parser as P exposing ((|.), (|=))
import Parser.Document exposing (Block, BlockType(..), LineType(..))


type alias BlockState =
    { input : List String
    , lineNumber : Int
    , generation : Int
    , blockType : BlockType
    , blockContents : List String
    , blockTypeStack : List BlockType
    , output : List (List String)
    }



--process : Int -> String -> List (List Expression)


compile : Int -> String -> List (List String)
compile generation input =
    (process generation input).output


process : Int -> String -> BlockState
process generation =
    runProcess generation



-- >> toParsed


{-| Compute the final BlockState of a string of source text.
The output field of BlockState holds the AST of the source text.

Function process operates a state machine which identifies logical
chunks of text, parses these using Parser.Parser.parseLoop,
and prepends them to a list of TextCursor.

-}
runProcess : Int -> String -> BlockState
runProcess generation str =
    loop (init generation str) nextState


init : Int -> String -> BlockState
init generation str =
    { input = String.lines str
    , lineNumber = 0
    , generation = generation
    , blockType = Start
    , blockContents = []
    , blockTypeStack = []
    , output = []
    }


nextState : BlockState -> Step BlockState BlockState
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
                ( Start, LTStart ) ->
                    Loop (start state)

                ( Start, LTMathBlock ) ->
                    Loop (initBlock MathBlock currentLine state)

                ( Start, BeginEnvBlock blockType ) ->
                    Loop (pushBlockStack (EnvBlock blockType) currentLine state)

                ( Start, EndEnvBlock _ ) ->
                    Loop (initBlock ErrorBlock currentLine state)

                ( Start, LTTextBlock ) ->
                    Loop (initBlock TextBlock currentLine state)

                --
                ( ErrorBlock, LTStart ) ->
                    Loop { state | blockType = Start, blockContents = [] }

                ( ErrorBlock, LTMathBlock ) ->
                    Loop (initWithBlockType MathBlock currentLine state)

                ( ErrorBlock, BeginEnvBlock blockType ) ->
                    Loop (initWithBlockType (EnvBlock blockType) currentLine state)

                ( ErrorBlock, EndEnvBlock _ ) ->
                    Loop (addToBlockContents ErrorBlock currentLine state)

                ( ErrorBlock, LTTextBlock ) ->
                    Loop (initWithBlockType TextBlock currentLine state)

                --
                ( TextBlock, LTStart ) ->
                    Loop { state | blockType = Start, blockContents = [], output = List.reverse state.blockContents :: state.output }

                ( TextBlock, LTMathBlock ) ->
                    Loop (initWithBlockType MathBlock currentLine state)

                ( TextBlock, BeginEnvBlock blockType ) ->
                    Loop (initWithBlockType (EnvBlock blockType) currentLine state)

                ( TextBlock, EndEnvBlock _ ) ->
                    Loop (addToBlockContents ErrorBlock currentLine state)

                ( TextBlock, LTTextBlock ) ->
                    Loop (addToBlockContents TextBlock currentLine state)

                --
                ( MathBlock, LTStart ) ->
                    Loop { state | blockType = Start, blockContents = [], output = List.reverse state.blockContents :: state.output }

                ( MathBlock, LTMathBlock ) ->
                    Loop (initWithBlockType Start currentLine state)

                ( MathBlock, BeginEnvBlock blockType ) ->
                    Loop (initWithBlockType (EnvBlock blockType) currentLine state)

                ( MathBlock, EndEnvBlock _ ) ->
                    Loop (addToBlockContents ErrorBlock currentLine state)

                ( MathBlock, LTTextBlock ) ->
                    Loop (addToBlockContents MathBlock currentLine state)

                ---
                ( EnvBlock _, LTStart ) ->
                    Loop state

                ( EnvBlock et, LTMathBlock ) ->
                    Loop (addToBlockContents (EnvBlock et) currentLine state)

                ( EnvBlock _, BeginEnvBlock blockType ) ->
                    Loop (pushBlockStack (EnvBlock blockType) currentLine state)

                ( EnvBlock et, EndEnvBlock _ ) ->
                    Loop (popBlockStack (EnvBlock et) currentLine state)

                ( EnvBlock et, LTTextBlock ) ->
                    Loop (addToBlockContents (EnvBlock et) currentLine state)



-- OPERATIONS ON STATE


{-| Put BlockState in the Start state
-}
start : BlockState -> BlockState
start state =
    { state | blockType = Start, blockTypeStack = [], blockContents = [] }


initBlock : BlockType -> String -> BlockState -> BlockState
initBlock blockType_ currentLine_ state =
    { state | blockType = blockType_, blockContents = [ currentLine_ ] }


initWithBlockType : BlockType -> String -> BlockState -> BlockState
initWithBlockType blockType_ currentLine_ state =
    { state
        | blockType = blockType_
        , blockContents = [ currentLine_ ]
        , lineNumber = state.lineNumber + countLines state.blockContents
        , output = state.blockContents :: state.output
    }


addToBlockContents : BlockType -> String -> BlockState -> BlockState
addToBlockContents blockType_ currentLine_ state =
    { state | blockType = blockType_, blockContents = currentLine_ :: state.blockContents }


pushBlockStack : BlockType -> String -> BlockState -> BlockState
pushBlockStack blockType_ currentLine_ state =
    { state
        | blockType = blockType_
        , blockTypeStack = blockType_ :: state.blockTypeStack
        , blockContents = currentLine_ :: state.blockContents
    }


popBlockStack : BlockType -> String -> BlockState -> BlockState
popBlockStack blockType_ currentLine_ state =
    let
        newBlockTypeStack =
            List.drop 1 state.blockTypeStack
    in
    if newBlockTypeStack == [] then
        let
            block =
                List.reverse (currentLine_ :: state.blockContents)
        in
        { state
            | blockType = Start
            , blockTypeStack = []
            , blockContents = currentLine_ :: state.blockContents
            , output = List.reverse block :: state.output
            , lineNumber = state.lineNumber + (2 + List.length state.blockContents) -- TODO: think about this.  Is it correct?
        }

    else
        { state
            | blockType = blockType_
            , blockTypeStack = newBlockTypeStack
            , blockContents = currentLine_ :: state.blockContents
        }


flush : BlockState -> BlockState
flush state =
    let
        block =
            List.reverse state.blockContents
    in
    { state | output = List.reverse (block :: state.output) }


countLines : List String -> Int
countLines list =
    list |> List.map String.lines |> List.concat |> List.length |> (\x -> x + 1)



-- HELPER (LOOP)


loop : BlockState -> (BlockState -> Step BlockState BlockState) -> BlockState
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
            LTStart


lineTypeParser =
    P.oneOf [ mathBlockParser, beginEnvLineParser, endEnvLineParser, textBlockParser, P.succeed LTStart ]


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
        |. P.symbol "$$"


textBlockParser : P.Parser LineType
textBlockParser =
    P.succeed LTTextBlock
        |. P.chompIf (\_ -> True)
