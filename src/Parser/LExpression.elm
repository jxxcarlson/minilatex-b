module Parser.LExpression exposing (..)



type Expression
    = Text String SourceMap
    | InlineMath String SourceMap
    | DisplayMath String SourceMap
    --| Macro String (Maybe String) (List String)
    | LXList (List Expression)
    | LXError String Problem SourceMap



getSource : Expression -> SourceMap
getSource expr =
    case expr of
        Text _ source -> source
        InlineMath _ source -> source
        DisplayMath _ source -> source
        LXError _ _ source -> source
        LXList e -> List.map getSource e |> List.head |> Maybe.withDefault {begin = -1, end = -1, offset = -1}

incrementOffset : Int -> Expression -> Expression
incrementOffset delta expr =
     case expr of
         Text e source -> Text e {source | offset = source.offset + delta}
         InlineMath e source -> InlineMath e {source | offset = source.offset + delta}
         DisplayMath e source -> DisplayMath e {source | offset = source.offset + delta}
         LXError e p source -> LXError e p {source | offset = source.offset + delta}
         LXList e -> LXList (List.map (incrementOffset delta) e)

type alias SourceMap =  {begin : Int, end: Int, offset: Int}


type Problem =
    ExpectingLeadingDollarSign
   | ExpectingTrailingDollarSign1
   | ExpectingTrailingDollarSign2
   | ExpectingLeadingDoubleDollarSign
   | ExpectingLTrailingDoubleDollarSign1
   | ExpectingLTrailingDoubleDollarSign2
   | EndOfInput
   | ExpectingEndOfWordSpace
   | ExpectingLeadingBackslashForMacro
   | GenericError


problemAsString : Problem -> String
problemAsString prob =
    case prob of
        ExpectingLeadingDollarSign -> "I was expecting a '$' to begin inline math"
        ExpectingTrailingDollarSign1 -> "I was expecting a matching '$' to end inline math (1)"
        ExpectingTrailingDollarSign2 -> "I was expecting a matching '$' to end inline math (2)"
        ExpectingLeadingDoubleDollarSign -> "I was expecting '$$' to begin display math"
        ExpectingLTrailingDoubleDollarSign1 -> "I was expecting a matching '$$' to end display math (1)"
        ExpectingLTrailingDoubleDollarSign2 -> "I was expecting a matching '$$' to end display math (2)"
        EndOfInput -> "Unexpected end of input"
        ExpectingLeadingBackslashForMacro -> "I was expecting '\\' to begin a macro"
        GenericError -> "Generic error -- I'm at a loss for wards"
        ExpectingEndOfWordSpace ->"Expecting a space to end a waord"
