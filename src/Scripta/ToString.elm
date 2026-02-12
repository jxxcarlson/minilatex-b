module Scripta.ToString exposing (..)

import Scripta.FromLaTeX
import Scripta.Types

toScripta : String -> String
toScripta str =
    str |> Scripta.FromLaTeX.convertFromString |> convert


convert : List Scripta.Types.ExpressionBlock -> String
convert ast =
    "NOT IMPLEMENTED"
