module Render.LaTeXState exposing (..)

import Dict
import Html exposing (Attribute)
import Html.Attributes as HA
import Parser.Expression exposing (Expression(..), SourceMap)



{- TYPES AND DEFAULT VALUES -}


type LaTeXMsg
    = SendSourceMap SourceMap


type alias LaTeXState =
    { counters : Counters
    , crossReferences : CrossReferences
    , tableOfContents : TableOfContents
    , dictionary : Dictionary
    , textMacroDictionary : MacroDictionary
    , mathMacroDictionary : MacroDictionary
    , config : Config
    }


type alias Config =
    { lineHeight : Attribute LaTeXMsg
    , textSpanStyle : List (Attribute LaTeXMsg)
    }


init : LaTeXState
init =
    emptyLatexState


defaultConfig =
    { lineHeight = HA.style "line-height" "1.5"
    , textSpanStyle = [ HA.style "line-height" "1.5" ]
    }


type alias Counters =
    Dict.Dict String Int


type alias CrossReferences =
    Dict.Dict String String


type alias TableOfContents =
    List TocEntry


type alias TocEntry =
    { name : String, label : String, level : Int }


type alias Dictionary =
    Dict.Dict String String


type alias MacroDictionary =
    Dict.Dict String Expression


emptyDict : Dict.Dict k v
emptyDict =
    Dict.empty


addSection : String -> String -> Int -> LaTeXState -> LaTeXState
addSection sectionName label level latexState =
    let
        newEntry =
            TocEntry sectionName label level

        toc =
            latexState.tableOfContents ++ [ newEntry ]
    in
    { latexState | tableOfContents = toc }


getCounter : String -> LaTeXState -> Int
getCounter name latexState =
    case Dict.get name latexState.counters of
        Just k ->
            k

        Nothing ->
            0


getCrossReference : String -> LaTeXState -> String
getCrossReference label latexState =
    case Dict.get label latexState.crossReferences of
        Just ref ->
            ref

        Nothing ->
            "??"


getDictionaryItem : String -> LaTeXState -> String
getDictionaryItem key latexState =
    case Dict.get key latexState.dictionary of
        Just value ->
            value

        Nothing ->
            ""


setDictionaryItem : String -> String -> LaTeXState -> LaTeXState
setDictionaryItem key value latexState =
    let
        dictionary =
            latexState.dictionary

        newDictionary =
            Dict.insert key value dictionary
    in
    { latexState | dictionary = newDictionary }



--setMathMacroDictionary : String -> LatexState -> LatexState
--setMathMacroDictionary str latexState =
--   { latexState | mathMacroDictionary = Internal.MathMacro.makeMacroDict str }


incrementCounter : String -> LaTeXState -> LaTeXState
incrementCounter name latexState =
    let
        maybeInc =
            Maybe.map (\x -> x + 1)

        newCounters =
            Dict.update name maybeInc latexState.counters
    in
    { latexState | counters = newCounters }


updateCounter : String -> Int -> LaTeXState -> LaTeXState
updateCounter name value latexState =
    let
        maybeSet =
            Maybe.map (\x -> value)

        newCounters =
            Dict.update name maybeSet latexState.counters
    in
    { latexState | counters = newCounters }


setCrossReference : String -> String -> LaTeXState -> LaTeXState
setCrossReference label value latexState =
    let
        crossReferences =
            latexState.crossReferences

        newCrossReferences =
            Dict.insert label value crossReferences
    in
    { latexState | crossReferences = newCrossReferences }


setMacroDefinition : String -> Expression -> LaTeXState -> LaTeXState
setMacroDefinition macroName macroDefinition latexState =
    let
        macroDictionary =
            latexState.textMacroDictionary

        newMacroDictionary =
            Dict.insert macroName macroDefinition macroDictionary
    in
    { latexState | textMacroDictionary = newMacroDictionary }


initialCounters : Dict.Dict String number
initialCounters =
    Dict.fromList [ ( "s1", 0 ), ( "s2", 0 ), ( "s3", 0 ), ( "tno", 0 ), ( "eqno", 0 ) ]


emptyLatexState : LaTeXState
emptyLatexState =
    { counters = initialCounters
    , crossReferences = Dict.empty
    , dictionary = Dict.empty
    , tableOfContents = []
    , textMacroDictionary = Dict.empty
    , mathMacroDictionary = Dict.empty
    , config = defaultConfig
    }