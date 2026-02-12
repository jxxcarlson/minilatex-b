module Scripta.Types exposing (..)

import Dict exposing (Dict)
import Either exposing (Either)


{-| GenericBlock: a parameterized block, i.e., a type constructor

  - PrimitiveBlocks: content = List String
  - ExpressionBlocks: content = Either String (List Expression)

-}
type alias GenericBlock content metaData style =
    { heading : Heading
    , indent : Int
    , args : List String
    , properties : Dict String String
    , firstLine : String
    , body : content
    , meta : metaData
    , style : style
    }


type Heading
    = Paragraph
    | Ordinary String -- block name
    | Verbatim String -- block name


{-| A block whose content is a list of expressions.
-}
type alias ExpressionBlock =
    GenericBlock (Either String (List Expression)) BlockMeta NullStyle



-- EXPRESSIONS


type alias Expression =
    Expr ExprMeta


type Expr metaData
    = Text String metaData
    | Fun String (List (Expr metaData)) metaData
    | VFun String String metaData
    | ExprList Int (List (Expr metaData)) metaData -- the Int parameter is the indentation of the expression list in the source



-- METADATA


type alias BlockMeta =
    { id : String
    , position : Int
    , lineNumber : Int
    , bodyLineNumber : Int
    , numberOfLines : Int
    , messages : List String
    , sourceText : String
    , error : Maybe String
    }


type alias ExprMeta =
    { begin : Int, end : Int, index : Int, id : String }



-- STYLE


type alias NullStyle =
    {}
