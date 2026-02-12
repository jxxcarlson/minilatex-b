module Scripta.ToString exposing (..)

import Scripta.FromLaTeX
import Scripta.Types

fromString : String -> String
fromString str =
    str |> Scripta.FromLaTeX.convertFromString |> fromAST


fromAST : List Scripta.Types.ExpressionBlock -> String
fromAST ast =
    "NOT IMPLEMENTED"
