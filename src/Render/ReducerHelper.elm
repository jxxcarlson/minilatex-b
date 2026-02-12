module Render.ReducerHelper exposing
    ( setDictionaryItemForMacro, setEquationNumber, setSectionCounters, setTheoremNumber
    , updateSectionNumber, updateSubsectionNumber, updateSubsubsectionNumber
    )

{-|

@docs setDictionaryItemForMacro, setEquationNumber, setSectionCounters, setTheoremNumber
@docs updateSectionNumber, updateSubsectionNumber, updateSubsubsectionNumber

-}

import Dict
import List.Extra
import Maybe.Extra
import Parser.Advanced
import Parser.Core as Parser
import Parser.Expression exposing (Expression(..))
import Render.LaTeXState
    exposing
        ( Counters
        , CrossReferences
        , LaTeXState
        , addSection
        , getCounter
        , incrementCounter
        , setCrossReference
        , setDictionaryItem
        , updateCounter
        )
import Render.TextMacro


{-| -}
updateSectionNumber : List Expression -> LaTeXState -> LaTeXState
updateSectionNumber macroArgs latexState =
    let
        label =
            getCounter "s1" latexState |> (\x -> x + 1) |> String.fromInt
    in
    latexState
        |> incrementCounter "s1"
        |> updateCounter "s2" 0
        |> updateCounter "s3" 0
        |> addSection (unpackString macroArgs) label 1


{-| -}
updateSubsectionNumber : List Expression -> LaTeXState -> LaTeXState
updateSubsectionNumber macroArgs latexState =
    let
        s1 =
            getCounter "s1" latexState |> String.fromInt

        s2 =
            getCounter "s2" latexState |> (\x -> x + 1) |> String.fromInt

        label =
            s1 ++ "." ++ s2
    in
    latexState
        |> incrementCounter "s2"
        |> updateCounter "s3" 0
        |> addSection (unpackString macroArgs) label 2


{-| -}
updateSubsubsectionNumber : List Expression -> LaTeXState -> LaTeXState
updateSubsubsectionNumber macroArgs latexState =
    let
        s1 =
            getCounter "s1" latexState |> String.fromInt

        s2 =
            getCounter "s2" latexState |> String.fromInt

        s3 =
            getCounter "s3" latexState |> (\x -> x + 1) |> String.fromInt

        label =
            s1 ++ "." ++ s2 ++ "." ++ s3
    in
    latexState
        |> incrementCounter "s3"
        |> addSection (unpackString macroArgs) label 2


{-| -}
setSectionCounters : List Expression -> LaTeXState -> LaTeXState
setSectionCounters macroArgs latexState =
    let
        arg1 : Maybe String
        arg1 =
            List.Extra.getAt 0 macroArgs |> Maybe.map Parser.Expression.toString

        arg2 : Maybe String
        arg2 =
            List.Extra.getAt 1 macroArgs |> Maybe.map Parser.Expression.toString

        initialSectionNumber =
            case ( arg1, arg2 ) of
                ( Just sectionName, Just sectionNumberAsString ) ->
                    if sectionName == "section" then
                        sectionNumberAsString |> String.toInt |> Maybe.withDefault 0

                    else
                        0

                _ ->
                    -1
    in
    if initialSectionNumber > -1 then
        latexState
            |> updateCounter "s1" (initialSectionNumber - 1)
            |> updateCounter "s2" 0
            |> updateCounter "s3" 0

    else
        latexState


{-| -}
setDictionaryItemForMacro : String -> List Expression -> LaTeXState -> LaTeXState
setDictionaryItemForMacro name args latexState =
    -- TODO: fix this!
    let
        value =
            Parser.renderArg args
    in
    setDictionaryItem name value latexState


{-| -}
setTheoremNumber : Expression -> LaTeXState -> LaTeXState
setTheoremNumber body latexState =
    let
        label =
            case body |> macroValue "label" of
                Just str ->
                    str

                Nothing ->
                    ""

        latexState1 =
            incrementCounter "tno" latexState

        tno =
            getCounter "tno" latexState1

        s1 =
            getCounter "s1" latexState1

        latexState2 =
            if label /= "" then
                setCrossReference label (String.fromInt s1 ++ "." ++ String.fromInt tno) latexState1

            else
                latexState1
    in
    latexState2


{-| -}
setEquationNumber : Expression -> LaTeXState -> LaTeXState
setEquationNumber body latexState =
    let
        label =
            case body of
                Text str _ ->
                    getLabel str

                _ ->
                    ""

        latexState1 =
            incrementCounter "eqno" latexState

        eqno =
            getCounter "eqno" latexState1

        s1 =
            getCounter "s1" latexState1

        latexState2 =
            if label /= "" then
                setCrossReference label (String.fromInt s1 ++ "." ++ String.fromInt eqno) latexState1

            else
                latexState1
    in
    latexState2


{-| -}
setBibItemXRef : List Expression -> List Expression -> LaTeXState -> LaTeXState
setBibItemXRef optionalArgs args latexState =
    let
        label =
            unpackString args

        value =
            if optionalArgs == [] then
                label

            else
                unpackString optionalArgs
    in
    setDictionaryItem ("bibitem:" ++ label) value latexState



--setMacroDefinition : String -> Expression -> LaTeXState -> LaTeXState
--setMacroDefinition name body latexState =
--    setMacroDefinition name body latexState
{- Helpers -}
--List.Extra.getAt : Int -> List String -> String
--List.Extra.getAt k list_ =
--   List.Extra.getAt k list_ |> Maybe.withDefault "xxx"


{-| -}
getElement : Int -> List Expression -> String
getElement k list =
    let
        lxString =
            List.Extra.getAt k list |> Maybe.withDefault (Text "xxx" Parser.Expression.dummySourceMap)
    in
    case lxString of
        Text str _ ->
            str

        _ ->
            "yyy"


{-| -}
getLabel str =
    let
        maybeMacro =
            str
                |> String.trim
                |> Parser.Advanced.run (Parser.macro 0 0)
    in
    case maybeMacro of
        Ok macro ->
            macro |> getFirstMacroArg "label"

        _ ->
            ""


{-| -}
getFirstMacroArg : String -> Expression -> String
getFirstMacroArg macroName latexExpression =
    let
        arg =
            getSimpleMacroArgs macroName latexExpression |> List.head
    in
    case arg of
        Just value ->
            value

        _ ->
            ""


{-| -}
getSimpleMacroArgs : String -> Expression -> List String
getSimpleMacroArgs macroName latexExpression =
    latexExpression
        |> getMacroArgs macroName
        |> List.map (List.head >> Maybe.map Parser.Expression.toString)
        |> Maybe.Extra.values


{-| -}
getMacroArgs : String -> Expression -> List (List Expression)
getMacroArgs macroName latexExpression =
    case latexExpression of
        Macro name optArgs args _ ->
            if name == macroName then
                args
                    |> List.map latexList2List

            else
                []

        _ ->
            []



-- HELPERS


{-| -}
headExpression : List Expression -> Expression
headExpression list =
    let
        he =
            case List.head list of
                Just expr ->
                    expr

                Nothing ->
                    LXList []
    in
    he


{-| -}
valueOfLatexList : Expression -> List Expression
valueOfLatexList latexList =
    case latexList of
        LXList value ->
            value

        _ ->
            [ Text "Error getting value of LatexList" Parser.Expression.dummySourceMap ]


{-| -}
valueOfLXString : Expression -> String
valueOfLXString expr =
    case expr of
        Text str _ ->
            str

        _ ->
            "Error getting value of LatexString"


{-| Get string arg
-}
unpackString : List Expression -> String
unpackString expr =
    -- TODO: verify this fix
    --expr |> headExpression |> valueOfLatexList |> headExpression |> valueOfLXString
    Parser.renderArg expr



--list2LeadingString : List a -> String
--list2LeadingString list =
--    let
--        head_ =
--            list |> List.head
--
--        value =
--            case head_ of
--                Just value_ ->
--                    value_
--
--                Nothing ->
--                    ""
--    in
--    value
--case value of
--    Text str _ ->
--        str
--
--    _ ->
--        ""


{-| -}
latexList2List : Expression -> List Expression
latexList2List latexExpression =
    case latexExpression of
        LXList list ->
            list

        _ ->
            []


{-| -}
macroValue : String -> Expression -> Maybe String
macroValue macroName envBody =
    case envBody of
        LXList list ->
            macroValue_ macroName list

        _ ->
            Nothing


{-| -}
macroValue_ : String -> List Expression -> Maybe String
macroValue_ macroName list =
    list
        |> filterMacro macroName
        |> List.head
        |> Maybe.map getMacroArgs2
        |> Maybe.andThen List.head
        |> Maybe.andThen List.head
        |> Maybe.map getString


{-| -}
getMacroArgs2 : Expression -> List (List Expression)
getMacroArgs2 latexExpression =
    case latexExpression of
        Macro name optArgs args _ ->
            args
                |> List.map latexList2List

        _ ->
            []


{-| -}
getString : Expression -> String
getString latexString =
    case latexString of
        Text str _ ->
            str

        _ ->
            ""


{-| -}
filterMacro : String -> List Expression -> List Expression
filterMacro macroName list =
    List.filter (isMacro macroName) list


{-| List.filter (isMacro "label") latexList returns
a list of macros with name "label"
-}
isMacro : String -> Expression -> Bool
isMacro macroName latexExpression =
    case latexExpression of
        Macro name _ _ _ ->
            name
                == macroName

        _ ->
            False
