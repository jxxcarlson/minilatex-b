module Parser.Document exposing (..)

import Parser as P exposing((|.), (|=))

import Parser.Parser as Parser

import Parser.TextCursor as TextCursor exposing(TextCursor)


type alias State = {
          input : List String
        , lineNumber : Int
        , blockType : BlockType
        , blockContents : List String
        , blockTypeStack : List BlockType
        , output: List TextCursor
        }



type alias Block = {blockType : BlockType, content: List String}

type BlockType = Start | TextBlock | MathBlock | EnvBlock String | ErrorBlock

type LineType = LTStart | LTTextBlock | LTMathBlock | BeginEnvBlock String | EndEnvBlock String

process : String -> State
process str = loop (init str) nextState

toParsed state = state.output |> List.map .parsed
toInput state = state.output |> List.map .text

init : String -> State
init str =  {
     input = String.lines str
   , lineNumber = 0
   , blockType = Start
   , blockContents = []
   , blockTypeStack = []
   , output = []
  }

nextState : State -> Step State State
nextState state_ =
    case List.head state_.input of
        Nothing ->  Done (flush state_)
        Just currentLine ->
            let
               state = {state_ | input = List.drop 1 state_.input}
            in
            case (state.blockType, classify currentLine) of
              (Start, LTStart) -> Loop (start state)
              (Start, LTMathBlock) -> Loop (initBlock MathBlock currentLine state)
              (Start, BeginEnvBlock blockType) -> Loop  (pushBlockStack (EnvBlock blockType) currentLine state )
              (Start, EndEnvBlock _) -> Loop (initBlock ErrorBlock currentLine state)
              (Start, LTTextBlock) -> Loop (initBlock TextBlock currentLine state)
              --
              (ErrorBlock, LTStart) ->
                Loop {state | blockType = Start, blockContents = []}
              (ErrorBlock, LTMathBlock) -> Loop (initWithBlockType MathBlock currentLine state                                          )
              (ErrorBlock, BeginEnvBlock blockType) -> Loop (initWithBlockType (EnvBlock blockType) currentLine state)
              (ErrorBlock, EndEnvBlock _) ->  Loop (addToBlockContents ErrorBlock currentLine state )
              (ErrorBlock, LTTextBlock) ->  Loop (initWithBlockType TextBlock currentLine state)
              --
              (TextBlock, LTStart) ->
                Loop {state | blockType = Start, blockContents = [], output = pushTC state,  lineNumber = state.lineNumber + (countLines state.blockContents)}
              (TextBlock, LTMathBlock) ->  Loop (initWithBlockType MathBlock currentLine state)
              (TextBlock, BeginEnvBlock blockType) -> Loop (initWithBlockType (EnvBlock blockType) currentLine state)
              (TextBlock, EndEnvBlock _) -> Loop (addToBlockContents ErrorBlock currentLine state )
              (TextBlock, LTTextBlock) -> Loop (addToBlockContents TextBlock currentLine state )
              --
              (MathBlock, LTStart) ->
                Loop {state | blockType = Start, blockContents = [], output = pushTC state,  lineNumber = state.lineNumber + (countLines state.blockContents)}
              (MathBlock, LTMathBlock) -> Loop (initWithBlockType Start currentLine state)
              (MathBlock, BeginEnvBlock blockType) ->  Loop (initWithBlockType (EnvBlock blockType) currentLine state)
              (MathBlock, EndEnvBlock _) -> Loop (addToBlockContents ErrorBlock currentLine state )
              (MathBlock, LTTextBlock) -> Loop (addToBlockContents MathBlock currentLine state )
              ---
              (EnvBlock et, LTStart) -> Loop state
              (EnvBlock et, LTMathBlock) -> Loop (addToBlockContents (EnvBlock et) currentLine state)
              (EnvBlock et, BeginEnvBlock blockType) -> Loop (pushBlockStack (EnvBlock blockType) currentLine state)
              (EnvBlock et, EndEnvBlock _) -> Loop (popBlockStack (EnvBlock et) currentLine state)
              (EnvBlock et, LTTextBlock) -> Loop (addToBlockContents (EnvBlock et) currentLine state )



start state =
    {state | blockType = Start,  blockTypeStack = [], blockContents = []}

initBlock blockType_ currentLine_ state =
    {state | blockType = blockType_, blockContents = [currentLine_] }

initWithBlockType blockType_ currentLine_ state =
    {state | blockType = blockType_, blockContents = [currentLine_]
      ,  lineNumber = state.lineNumber + (countLines state.blockContents)
      , output = pushTC2 currentLine_ state
      }

countLines : List String -> Int
countLines list =
    list |> List.map String.lines |> List.concat |> List.length |> (\x -> x + 1)

addToBlockContents blockType_ currentLine_ state =
    {state | blockType = blockType_, blockContents = currentLine_::state.blockContents }

pushBlockStack blockType_ currentLine_ state =
    {state | blockType = blockType_
           , blockTypeStack = blockType_::state.blockTypeStack
           , blockContents = currentLine_::state.blockContents }

popBlockStack : BlockType -> String -> State -> State
popBlockStack blockType_ currentLine_ state =
  let
      newBlockTypeStack = List.drop 1 state.blockTypeStack
  in
    if newBlockTypeStack == [] then
        let
            input_ =  String.join "\n" (List.reverse (currentLine_::state.blockContents)) |> Debug.log "input_"
            tc_ = Parser.parseLoop state.lineNumber input_
            tc = {tc_ | text = input_}
        in
        {state | blockType = Start
           , blockTypeStack = []
           , blockContents = currentLine_::state.blockContents
           , output = tc::state.output
         }
    else
      {state | blockType = blockType_
         , blockTypeStack = newBlockTypeStack
         , blockContents = currentLine_::state.blockContents }



pushTC : State -> List TextCursor
pushTC state =
    (Parser.parseLoop state.lineNumber (String.join "\n" ( List.reverse state.blockContents)))::state.output

pushTC2 : String -> State -> List TextCursor
pushTC2 str state =
    (Parser.parseLoop state.lineNumber (String.join "\n" ( List.reverse (str::state.blockContents))))::state.output

flush : State -> State
flush state =
   let
     input =  String.join "\n" (List.reverse state.blockContents)
     tc_ = Parser.parseLoop state.lineNumber input
     tc = {tc_ | text = input}
   in
    { state | output = List.reverse (tc::state.output)}

-- HELPER (LOOP)

loop : State -> (State -> Step State State) -> State
loop s nextState_ =
    case nextState_ s of
        Loop s_ ->
            let
              _ = Debug.log "ST" (List.length s_.input, s_.blockType)
            in
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
        Ok lt -> lt
        Err _ -> LTStart

lineTypeParser = P.oneOf [mathBlockParser, beginEnvLineParser, endEnvLineParser, textBlockParser, P.succeed LTStart]

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
    P.succeed  LTMathBlock
      |. P.symbol "$$"

textBlockParser : P.Parser LineType
textBlockParser =
    P.succeed LTTextBlock
      |. P.chompIf (\c -> True)
