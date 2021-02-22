module Parser.Paragraph exposing (..)

import Parser as P exposing((|.), (|=))

import Parser.Parser as Parser

import Parser.TextCursor as TextCursor exposing(TextCursor)

type alias State = {
          input : List String
        , blockType : BlockType
        , blockContents : List String
        , blockTypeStack : List BlockType
        , output: List TextCursor
        }

type alias Block = {blockType : BlockType, content: List String}

type BlockType = Start | TextBlock | MathBlock | EnvBlock String | ErrorBlock

type LineType = LTStart | LTTextBlock | LTMathBlock | BeginEnvBlock String | EndEnvBlock String


-- type Classification = Blankline | TextLine | BeginMathBlock | BeginEnvironment String

popStack : List String -> List String
popStack list = List.drop 1 list

pushStack : String -> List String -> List String
pushStack str list = str::list

--classifyEnvironment : String -> String
--classifyEnvironment str =
--    case Parser.run envLineParer str of
--        Ok classification -> classification
--        Err _ -> "env"

--envLineParser : Parser.Parser String
--envLineParer =
--   Parser.succeed identity
--      |. Parser.symbol "\\begin{"
--      |= Parser.getChompedString (Parser.chompUntil "}")

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


pushTC : State -> List TextCursor
pushTC state =
    (Parser.parseLoop (String.join "\n" state.blockContents))::state.output

pushTCS : State -> State
pushTCS state =
    { state | output = (Parser.parseLoop (String.join "\n" state.blockContents))::state.output}


initWithBlockType blockType_ currentLine_ state =
    {state | blockType = blockType_, blockContents = [currentLine_],  output = pushTC state }


addToBlockContents blockType_ currentLine_ state =
    {state | blockType = blockType_, blockContents = currentLine_::state.blockContents }

pushBlockStack blockType_ currentLine_ state =
    {state | blockType = blockType_
           , blockTypeStack = blockType_::state.blockTypeStack
           , blockContents = currentLine_::state.blockContents }

popBlockStack blockType_ currentLine_ state =
  let
      newBlockTypeStack = List.drop 1 state.blockTypeStack
  in
    if newBlockTypeStack == [] then
        {state | blockType = blockType_
           , blockTypeStack = newBlockTypeStack
           , blockContents = currentLine_::state.blockContents
         } |> pushTCS
    else
      {state | blockType = blockType_
         , blockTypeStack = newBlockTypeStack
         , blockContents = currentLine_::state.blockContents }


nextState : State -> Step State State
nextState state_ =
    case List.head state_.input of
        Nothing -> Done state_
        Just currentLine ->
            let
               state = {state_ | input = popStack state_.input}
            in
            case (state.blockType, classify currentLine) of
              (Start, LTStart) ->
                 Loop {state | blockType = Start
                             , blockTypeStack = [], blockContents = []}
              (Start, LTMathBlock) ->
                Loop {state | blockType = MathBlock
                            , blockContents = [currentLine] }
              (Start, BeginEnvBlock blockType) ->
                Loop  (pushBlockStack (EnvBlock blockType) currentLine state )
              (Start, EndEnvBlock _) ->
                Loop {state_| blockType = ErrorBlock
                            , blockContents = [currentLine] }
              (Start, LTTextBlock) ->
                Loop {state | blockType = TextBlock
                            , blockContents = [currentLine] }
              --
              (ErrorBlock, LTStart) ->
                Loop {state | blockType = Start, blockContents = []}
              (ErrorBlock, LTMathBlock) ->
                Loop (initWithBlockType MathBlock currentLine state)
              (ErrorBlock, BeginEnvBlock blockType) ->
                Loop (initWithBlockType (EnvBlock blockType) currentLine state)
              (ErrorBlock, EndEnvBlock _) ->
                Loop (addToBlockContents ErrorBlock currentLine state )
              (ErrorBlock, LTTextBlock) ->
                Loop (initWithBlockType TextBlock currentLine state)
              --
              (TextBlock, LTStart) ->
                Loop {state | blockType = Start, blockContents = [], output = pushTC state}
              (TextBlock, LTMathBlock) ->
                Loop (initWithBlockType MathBlock currentLine state)
              (TextBlock, BeginEnvBlock blockType) ->
                Loop (initWithBlockType (EnvBlock blockType) currentLine state)
              (TextBlock, EndEnvBlock _) ->
                Loop (addToBlockContents ErrorBlock currentLine state )
              (TextBlock, LTTextBlock) ->
                Loop (addToBlockContents TextBlock currentLine state )
              --
              (MathBlock, LTStart) ->
                Loop {state | blockType = Start, blockContents = [], output = pushTC state}
              (MathBlock, LTMathBlock) ->
                Loop (initWithBlockType Start currentLine state)
              (MathBlock, BeginEnvBlock blockType) ->
                Loop (initWithBlockType (EnvBlock blockType) currentLine state)
              (MathBlock, EndEnvBlock _) ->
                Loop (addToBlockContents ErrorBlock currentLine state )
              (MathBlock, LTTextBlock) ->
                Loop (initWithBlockType TextBlock currentLine state )
              ---
              (EnvBlock et, LTStart) ->
                Loop state
              (EnvBlock et, LTMathBlock) ->
                Loop (addToBlockContents (EnvBlock et) currentLine state)
              (EnvBlock et, BeginEnvBlock blockType) -> -- HHHH
                Loop (pushBlockStack (EnvBlock blockType) currentLine state)
              (EnvBlock et, EndEnvBlock _) ->
                Loop (popBlockStack (EnvBlock et) currentLine state)
              (EnvBlock et, LTTextBlock) ->
                Loop (addToBlockContents (EnvBlock et) currentLine state )


loop : state -> (state -> Step state a) -> a
loop s nextState_ =
    case nextState_ s of
        Loop s_ ->
            loop s_ nextState_

        Done b ->
            b

type Step state a
    = Loop state
    | Done a

