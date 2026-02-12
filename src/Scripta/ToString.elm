module Scripta.ToString exposing (..)

import Scripta.FromLaTeX
import Scripta.Types

convertFromString : String -> String
convertFromString str =
    str |> Scripta.FromLaTeX.convertFromString |> convertFromAST


convertFromAST : List Scripta.Types.ExpressionBlock -> String
convertFromAST ast =
    "NOT IMPLEMENTED"
