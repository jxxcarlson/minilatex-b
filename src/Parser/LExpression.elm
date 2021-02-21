module Parser.LExpression exposing (..)


type Expression
    = Text String
    | InlineMath String
    | DisplayMath String
    --| Macro String (Maybe String) (List String)
    | LXList (List Expression)


type Problem =
    ExpectingLeadingDollarSign
   | ExpectingTrailingDollarSign1
   | ExpectingTrailingDollarSign2
   | ExpectingLeadingDoubleDollarSign
   | ExpectingLTrailingDoubleDollarSign1
   | ExpectingLTrailingDoubleDollarSign2
   | EndOfInput
   | ExpectingLeadingBackslashForMacro
