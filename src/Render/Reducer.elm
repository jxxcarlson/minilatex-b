module Render.Reducer exposing (..)

import Parser.Expression exposing (Expression(..))
import Render.LaTeXState exposing (LaTeXState)
import Render.ReducerHelper as ReducerHelper


latexStateReducer : List Expression -> LaTeXState -> LaTeXState
latexStateReducer list state =
    List.foldr latexStateReducerAux state list


latexStateReducerAux : Expression -> LaTeXState -> LaTeXState
latexStateReducerAux lexpr state =
    case lexpr of
        --Macro name optionalArgs args sm ->
        --    macroReducer name optionalArgs args state
        --SMacro name optionalArgs args latexExpression ->
        --    smacroReducer name optionalArgs args latexExpression state
        --NewCommand name nArgs body ->
        --    ReducerHelper.setMacroDefinition name body state
        Environment name optonalArgs body sm ->
            envReducer name optonalArgs body state

        LXList list ->
            List.foldr latexStateReducerAux state list

        _ ->
            state


envReducer : String -> List Expression -> Expression -> LaTeXState -> LaTeXState
envReducer name optonalArgs body state =
    if List.member name theoremWords then
        ReducerHelper.setTheoremNumber body state

    else
        case name of
            "equation" ->
                ReducerHelper.setEquationNumber body state

            "align" ->
                ReducerHelper.setEquationNumber body state

            --"mathmacro" ->
            --    case body of
            --        LXString str ->
            --            let
            --                mathDict =
            --                    Internal.MathMacro.makeMacroDict (String.trim str)
            --            in
            --            { state | mathMacroDictionary = mathDict }
            --
            --        _ ->
            --            state
            --
            --"textmacro" ->
            --    case body of
            --        LXString str ->
            --            ReducerHelper.setDictionary str state

                    _ ->
                        state

            _ ->
                state



{-

   > env3
   LatexList [Macro "label" [] [LatexList [LXString "foo"]],LXString ("ho  ho  ho ")]
       : Expression

   > latexStateReducerAux env2 emptyLatexState
   { counters = Dict.fromList [("eqno",0),("s1",0),("s2",0),("s3",0),("tno",1)]
   , crossReferences = Dict.fromList [("foo","0.1")], dictionary = Dict.fromList []
   , macroDictionary = Dict.fromList [], tableOfContents = [] }

-}


theoremWords =
    [ "theorem", "proposition", "corollary", "lemma", "definition", "problem" ]


dictionaryWords =
    [ "title", "author", "date", "email", "revision", "host", "setclient", "setdocid" ]


macroReducer : String -> List Expression -> List Expression -> LaTeXState -> LaTeXState
macroReducer name optionalArgs args state =
    if List.member name dictionaryWords then
        ReducerHelper.setDictionaryItemForMacro name args state

    else
        case name of
            "section" ->
                ReducerHelper.updateSectionNumber args state

            "subsection" ->
                ReducerHelper.updateSubsectionNumber args state

            "subsubsection" ->
                ReducerHelper.updateSubsubsectionNumber args state

            "setcounter" ->
                ReducerHelper.setSectionCounters args state

            _ ->
                state



--smacroReducer : String -> List Expression -> List Expression -> Expression -> LaTeXState -> LaTeXState
--smacroReducer name optionalArgs args latexExpression state =
--    case name of
--        "bibitem" ->
--            ReducerHelper.setBibItemXRef optionalArgs args state
--
--        _ ->
--            state
