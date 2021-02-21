module Parser.LExpression exposing (..)



type Expression
    = Text String SourceMap
    | InlineMath String SourceMap
    | DisplayMath String SourceMap
    --| Macro String (Maybe String) (List String)
    | LXList (List Expression)


getSource : Expression -> SourceMap
getSource expr =
    case expr of
        Text _ source -> source
        InlineMath _ source -> source
        DisplayMath _ source -> source
        LXList e -> List.map getSource e |> List.head |> Maybe.withDefault {first = -1, last = -1, offset = -1}

incrementOffset : Int -> Expression -> Expression
incrementOffset delta expr =
     case expr of
         Text e source -> Text e {source | offset = source.offset + delta}
         InlineMath e source -> InlineMath e {source | offset = source.offset + delta}
         DisplayMath e source -> DisplayMath e {source | offset = source.offset + delta}
         LXList e -> LXList (List.map (incrementOffset delta) e)

type alias SourceMap =  {first : Int, last: Int, offset: Int}


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
