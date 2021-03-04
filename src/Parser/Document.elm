module Parser.Document exposing (process)

{-| The main function in this module is process, which takes as input
a string representing a MiniLaTeX document and produces as output
a value of type AST = List (List Expression).

@docs process

-}

import Parser as P exposing ((|.), (|=))
import Parser.Expression exposing (Expression)
import Parser.Parser as Parser
import Parser.TextCursor exposing (TextCursor)


type alias State =
    { input : List String
    , lineNumber : Int
    , blockType : BlockType
    , blockContents : List String
    , blockTypeStack : List BlockType
    , output : List TextCursor
    }


type alias Block =
    { blockType : BlockType, content : List String }


type BlockType
    = Start
    | TextBlock
    | MathBlock
    | EnvBlock String
    | ErrorBlock


type LineType
    = LTStart
    | LTTextBlock
    | LTMathBlock
    | BeginEnvBlock String
    | EndEnvBlock String


{-| Compute the AST of a string of source text.

    process =
        runProcess >> toParsed

Function runProcess operates a state machine which identifies logical
chunks of text, parses these using Parser.Parser.parseLoop,
and prepends them to a list of TextCursor. The function
toParsed extracts the AST from State.

-}
process : String -> List (List Expression)
process =
    runProcess >> toParsed


{-| Compute the final State of a string of source text.
The output field of State holds the AST of the source text.

Function process operates a state machine which identifies logical
chunks of text, parses these using Parser.Parser.parseLoop,
and prepends them to a list of TextCursor.

-}
runProcess : String -> State
runProcess str =
    loop (init str) nextState


{-| Return the AST from the State.
-}
toParsed : State -> List (List Expression)
toParsed state =
    state.output |> List.map .parsed


toParsed2 state =
    state.output |> List.length |> String.fromInt |> (\x -> "Lines: " ++ x)


toInput state =
    state.output |> List.map .text


init : String -> State
init str =
    { input = String.lines str
    , lineNumber = 0
    , blockType = Start
    , blockContents = []
    , blockTypeStack = []
    , output = []
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
                    Loop { state | blockType = Start, blockContents = [], output = pushTC state, lineNumber = state.lineNumber + countLines state.blockContents }

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
                    Loop { state | blockType = Start, blockContents = [], output = pushTC state, lineNumber = state.lineNumber + countLines state.blockContents }

                ( MathBlock, LTMathBlock ) ->
                    Loop (initWithBlockType Start currentLine state)

                ( MathBlock, BeginEnvBlock blockType ) ->
                    Loop (initWithBlockType (EnvBlock blockType) currentLine state)

                ( MathBlock, EndEnvBlock _ ) ->
                    Loop (addToBlockContents ErrorBlock currentLine state)

                ( MathBlock, LTTextBlock ) ->
                    Loop (addToBlockContents MathBlock currentLine state)

                ---
                ( EnvBlock et, LTStart ) ->
                    Loop state

                ( EnvBlock et, LTMathBlock ) ->
                    Loop (addToBlockContents (EnvBlock et) currentLine state)

                ( EnvBlock et, BeginEnvBlock blockType ) ->
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
    { state
        | blockType = blockType_
        , blockContents = [ currentLine_ ]
        , lineNumber = state.lineNumber + countLines state.blockContents
        , output = pushTC2 currentLine_ state
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
                Parser.parseLoop state.lineNumber input_

            tc =
                { tc_ | text = input_ }
        in
        { state
            | blockType = Start
            , blockTypeStack = []
            , blockContents = currentLine_ :: state.blockContents
            , output = tc :: state.output
            , lineNumber = state.lineNumber + Debug.log "BN" (2 + List.length state.blockContents) -- TODO: think about this.  Is it correct?
        }

    else
        { state
            | blockType = blockType_
            , blockTypeStack = newBlockTypeStack
            , blockContents = currentLine_ :: state.blockContents
        }


pushTC : State -> List TextCursor
pushTC state =
    Parser.parseLoop state.lineNumber (String.join "\n" (List.reverse state.blockContents)) :: state.output


pushTC2 : String -> State -> List TextCursor
pushTC2 str state =
    Parser.parseLoop state.lineNumber (String.join "\n" (List.reverse (str :: state.blockContents))) :: state.output


flush : State -> State
flush state =
    let
        input =
            String.join "\n" (List.reverse state.blockContents)

        tc_ =
            Parser.parseLoop state.lineNumber input

        tc =
            { tc_ | text = input }
    in
    { state | output = List.reverse (tc :: state.output) }


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
        |. P.chompIf (\c -> True)
