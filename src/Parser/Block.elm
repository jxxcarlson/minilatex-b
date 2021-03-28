module Parser.Block exposing
    ( compile
    , closestMatch, mathBlockParser, process
    )

{-| Parser.Block.compile takes an integer representing a "generation number"
and a string representing MiniLaTeX text and groups it into blocks of lines,
each block representing a "logical paragraph": either a true paragraph, that is,
non-blank lines surrounded by at least one blank line, or outer begin-end
pairs for a LaTeX environement.

@docs compile

-}

import Levenshtein
import List.Extra
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
-- TODO: should not blank lines be preserved when processing a begin-end block?
-- TODO: tests for compile!!


{-|

    > compile 0 "Pythagoras said that\n\n$$a^2 + b^2 = c^2$$\n\n\none\ntwo\nthree"
    [["Pythagoras said that"],["$$a^2 + b^2 = c^2$$"],["one","two","three"]]
        : List (List String)

    > compile 0 "\\begin{foo}\na\n\nb\nc\n\n\\end{foo}\n\n\none\ntwo\nthree"
    [["\\begin{foo}","a","b","c","\\end{foo}"],["one","two","three"]]
       : List (List String)

-}
compile : Int -> List String -> List (List String)
compile generation input =
    (process generation input).output


process : Int -> List String -> BlockState
process generation =
    runProcess generation



-- >> toParsed


{-| Compute the final BlockState of a string of source text.
The output field of BlockState holds the AST of the source text.

Function process operates a state machine which identifies logical
chunks of text, parses these using Parser.Parser.parseLoop,
and prepends them to a list of TextCursor.

-}
runProcess : Int -> List String -> BlockState
runProcess generation str =
    loop (init generation str) nextState
        |> edit


{-| Attempt to recover from error in which the final blockTypeStack is nonempty
-}
edit : BlockState -> BlockState
edit blockState =
    case List.head blockState.blockTypeStack of
        Nothing ->
            blockState
                |> Debug.log "BLOCKS OK, FINAL STATE"

        Just blockError ->
            case blockError of
                EnvBlock str ->
                    let
                        _ =
                            Debug.log "STATE" blockState

                        target =
                            "\\end{" ++ str ++ "}" |> Debug.log "TARGET"

                        m =
                            closestMatch target blockState.blockContents |> Debug.log "MATCH"

                        itemToReplace =
                            m |> Maybe.map .content |> Maybe.withDefault ""

                        index_ =
                            Maybe.map .index m |> Maybe.withDefault 0

                        blocksToReprocess =
                            List.take index_ blockState.blockContents |> Debug.log "BLOCKS TO REPROCESS"

                        rewrittenBlock =
                            List.Extra.setIf (\item -> item == itemToReplace) target (List.drop index_ blockState.blockContents) |> List.reverse |> Debug.log "REWRITTEN BLOCK"

                        _ =
                            Debug.log "OUTPUT" blockState.output

                        outputLength =
                            List.length blockState.output

                        truncatedOutput =
                            List.take (outputLength - List.length blockState.blockContents) blockState.output ++ [ rewrittenBlock ] |> Debug.log "REWRITTEN OUTPUT"

                        trailingState =
                            runProcess blockState.generation blocksToReprocess

                        indexOfBlockContents =
                            List.Extra.elemIndex (List.reverse blockState.blockContents) blockState.output
                                |> Debug.log "index of block contents"

                        prefix =
                            case List.Extra.elemIndex (List.reverse blockState.blockContents) blockState.output of
                                Nothing ->
                                    []

                                Just idx ->
                                    List.take idx blockState.output
                    in
                    { trailingState | output = prefix ++ truncatedOutput ++ trailingState.output }

                _ ->
                    blockState


closestMatch : String -> List String -> Maybe { distance : Int, index : Int, content : String }
closestMatch str list =
    List.indexedMap (\k s -> { distance = Levenshtein.distance str s, index = k, content = s }) list
        |> List.Extra.minimumBy (\r -> r.distance)


init : Int -> List String -> BlockState
init generation strList =
    { input = strList
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
                    Loop { state | blockType = Start, blockContents = [], output = List.reverse state.blockContents :: state.output }

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
                    Loop { state | blockType = Start, blockContents = [], output = List.reverse state.blockContents :: state.output }

                ( MathBlock, LTMathBlock _ ) ->
                    Loop (addToBlockContents Start currentLine state |> pushBlockReversed)

                ( MathBlock, BeginEnvBlock blockType ) ->
                    Loop (initWithBlockType (EnvBlock blockType) currentLine state)

                ( MathBlock, EndEnvBlock _ ) ->
                    Loop (addToBlockContents ErrorBlock currentLine state)

                ( MathBlock, LTTextBlock ) ->
                    Loop (addToBlockContents MathBlock currentLine state)

                ---
                ( EnvBlock et, LTBlank ) ->
                    Loop (addToBlockContents (EnvBlock et) currentLine state)

                ( EnvBlock et, LTMathBlock _ ) ->
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


pushBlock : BlockState -> BlockState
pushBlock state =
    { state | blockContents = [], output = state.blockContents :: state.output }


pushBlockReversed : BlockState -> BlockState
pushBlockReversed state =
    { state | blockContents = [], output = List.reverse state.blockContents :: state.output }


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
                currentLine_ :: state.blockContents
        in
        { state
            | blockType = Start
            , blockTypeStack = []
            , blockContents = []
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
    if block == [] then
        { state | output = List.reverse state.output }

    else
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


{-| type LineType
= LTStart -- always succeeds
| LTTextBlock -- always succeeds
| LTMathBlock -- starts with $$
| BeginEnvBlock String -- starts with \\begin{
| EndEnvBlock String -- starts with \\end{
-}
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
