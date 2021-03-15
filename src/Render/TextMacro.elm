module Render.TextMacro exposing (expandMacro, setMacroDefinition, setMacroDictionary)

{-| Expand text-mode macros

@docs expandMacro, setMacroDefinition, setMacroDictionary

-}

import Dict
import Parser.Expression exposing (Expression(..))
import Parser.Parser
import Render.LaTeXState exposing (LaTeXState)


{-| EXAMPLE

    import Parser exposing(run)
    import Internal.Parser exposing(..)
    import Internal.Macro exposing(..)

    run latexExpression "\\newcommand{\\hello}[1]{Hello \\strong{#1}!}"
    --> Ok (NewCommand "hello" 1 (LatexList [LXString ("Hello "),Macro "strong" [] [LatexList [LXString "#1"]],LXString "!"]))

    run latexExpression "\\hello{John}"
    --> Ok (Macro "hello" [] [LatexList [LXString "John"]])

    macroDef : Expression
    macroDef = NewCommand "hello" 1 (LatexList [LXString ("Hello "),Macro "strong" [] [LatexList [LXString "#1"]],LXString "!"])

    macro : Expression
    macro = Macro "hello" [] [LatexList [LXString "John"]]

    expandMacro macro macroDef
    --> LatexList [LXString ("Hello "),Macro "strong" [] [LatexList [LXString "John"]],LXString "!"]

-}
expandMacro : Expression -> Expression -> Expression
expandMacro macro macroDef =
    case expandMacro_ macro (NewCommand "foo" 13 macroDef Parser.Expression.dummySourceMap) of
        NewCommand _ _ latexList _ ->
            latexList

        _ ->
            LXError "!! Error expanding macro" Parser.Expression.UnHandledError Parser.Expression.dummySourceMap


expandMacro_ : Expression -> Expression -> Expression
expandMacro_ macro macroDef =
    case macroDef of
        Comment str sm ->
            Comment str sm

        Macro name optArgs args sm ->
            Macro name optArgs (List.map (expandMacro_ macro) args) sm

        --SMacro name optArgs args le ->
        --    SMacro name optArgs (List.map (expandMacro_ macro) args) (expandMacro_ macro le)
        --Item level latexExpr ->
        --    Item level (expandMacro_ macro latexExpr)
        InlineMath str sm ->
            InlineMath str sm

        DisplayMath str sm ->
            DisplayMath str sm

        Environment name args body sm ->
            Environment name args (expandMacro_ macro body) sm

        LXList latexList ->
            LXList (List.map (expandMacro_ macro) latexList)

        Text str sm ->
            Text (substitute macro str) sm

        NewCommand commandName numberOfArgs commandBody sm ->
            NewCommand commandName numberOfArgs (expandMacro_ macro commandBody) sm

        LXError error problem sm ->
            LXError error problem sm

        LXInstruction instr sm ->
            LXInstruction instr sm



-- SUBSTITUTION


substitute : Expression -> String -> String
substitute macro str =
    substituteMany (nArgs macro) macro str


substituteOne : Int -> Expression -> String -> String
substituteOne k macro str =
    let
        arg =
            renderArg k macro

        hashK =
            "#" ++ String.fromInt k
    in
    String.replace hashK arg str


nArgs : Expression -> Int
nArgs latexExpression =
    case latexExpression of
        Macro name optArgs args _ ->
            List.length args

        _ ->
            0


substituteMany : Int -> Expression -> String -> String
substituteMany k macro str =
    if k == 0 then
        str

    else
        substituteMany (k - 1) macro (substituteOne k macro str)


renderArg : Int -> Expression -> String
renderArg k macro =
    case macro of
        Macro name optArgs args _ ->
            renderArg_ (k - 1) args

        _ ->
            ""


renderArg_ : Int -> List Expression -> String
renderArg_ k expressions =
    Parser.Parser.getStringAtWithDefault k "ARG" (Parser.Parser.renderToStringList expressions)



-- DICTIONARY


{-| Take a string of text-mode macro definitions, parse them,
and add them to latexState.macrodictionary
-}
setMacroDictionary : String -> LaTeXState -> LaTeXState
setMacroDictionary str latexState =
    setDictionaryAux (Parser.Parser.parse str) latexState


setDictionaryAux : List Expression -> LaTeXState -> LaTeXState
setDictionaryAux list latexState =
    List.foldl macroDictReducer latexState list


macroDictReducer : Expression -> LaTeXState -> LaTeXState
macroDictReducer lexpr state =
    case lexpr of
        NewCommand name nArgs_ body _ ->
            setMacroDefinition name body state

        _ ->
            state


{-| -}
setMacroDefinition : String -> Expression -> LaTeXState -> LaTeXState
setMacroDefinition macroName macroDefinition latexState =
    let
        macroDictionary =
            latexState.textMacroDictionary
    in
    { latexState | textMacroDictionary = Dict.insert macroName macroDefinition macroDictionary }



------------ xxxx ------
-- FOR TEXTMACRO DICTONARY
--
--{-| Take a string of text-mode macro definitions, parse them,
--and add them to latexState.macrodictionary
---}
--setMacroDictionary : String -> LaTeXState -> LaTeXState
--setMacroDictionary str latexState =
--    case Parser.parseExpression 0 0 str of
--        Just e ->
--            -- TODO: I am pretty sure the below is wrong, even if it compiles.
--            setDictionaryAux [ e ] latexState
--
--        Nothing ->
--            latexState
--
--
--{-| -}
--setDictionaryAux : List Expression -> LaTeXState -> LaTeXState
--setDictionaryAux list latexState =
--    List.foldl macroDictReducer latexState list
--
--
--{-| -}
--macroDictReducer : Expression -> LaTeXState -> LaTeXState
--macroDictReducer lexpr state =
--    case lexpr of
--        NewCommand name nArgs body _ ->
--            Render.TextMacro.setMacroDefinition name body state
--
--        _ ->
--            state
