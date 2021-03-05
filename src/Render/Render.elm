module Render.Render exposing (..)

import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Json.Encode
import List.Extra
import Parser.Expression exposing (Expression(..), SourceMap)
import Parser.Helpers
import Render.LaTeXState as LaTeXState exposing (LaTeXMsg(..), LaTeXState)
import Utility


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


render : String -> LaTeXState -> List Expression -> List (Html LaTeXMsg)
render selectedId state exprs =
    List.map (renderExpr selectedId state) exprs


clicker sm =
    onClick (SendSourceMap sm)


id sm =
    String.fromInt sm.generation ++ "-" ++ String.fromInt sm.chunkOffset ++ "-" ++ String.fromInt sm.offset


active : SourceMap -> String -> List (Attribute LaTeXMsg)
active sm selectedId =
    let
        id_ =
            id sm
    in
    [ clicker sm, HA.id id_, highlight "#FAA" selectedId id_ ]


highlight : String -> String -> String -> Attribute LaTeXMsg
highlight color selectedId id_ =
    if id_ == selectedId then
        HA.style "background-color" color

    else
        HA.style "background-color" "clear"


renderExpr : String -> LaTeXState -> Expression -> Html LaTeXMsg
renderExpr selectedId state expr =
    case expr of
        Text s sm ->
            Html.span (state.config.textSpanStyle :: active sm selectedId) [ Html.text s ]

        InlineMath s sm ->
            inlineMathText selectedId s sm

        DisplayMath s sm ->
            displayMathText selectedId s sm

        Macro name optArg args sm ->
            macro selectedId state name optArg args sm

        Environment name args body sm ->
            environment selectedId state name args body sm

        LXList list_ ->
            List.map (renderExpr selectedId state) list_ |> Html.span [ state.config.textSpanStyle ]

        LXError s p sm ->
            Html.span [ clicker { sm | offset = sm.offset - sm.length } ]
                [ Html.span (active sm selectedId ++ state.config.errorStyle2) [ Html.text s ]
                , Html.span (active sm selectedId ++ state.config.errorStyle) [ Html.text (errorString p sm) ]
                ]

        LXInstruction _ _ ->
            Html.span [] [ Html.text " " ]


errorString : Parser.Expression.Problem -> Parser.Expression.SourceMap -> String
errorString p sm =
    " << "
        ++ Parser.Expression.problemAsString p


macro : String -> LaTeXState -> String -> Maybe String -> List Expression -> SourceMap -> Html LaTeXMsg
macro selectedId state name optArg args sm =
    case Dict.get name macroDict of
        Nothing ->
            undefinedMacro name sm

        Just f ->
            f selectedId state optArg args sm


undefinedMacro : String -> SourceMap -> Html LaTeXMsg
undefinedMacro name sm =
    Html.span [ HA.id (id sm), clicker sm, HA.style "color" "red" ] [ Html.text "Undefined macro: " ]


environment selectedId state name args body sm =
    renderEnvironment selectedId state name args body



--
-- BEGIN: RENDER ENVIRONMENT
-- RENDER ENVIRONMENTS


renderEnvironment : String -> LaTeXState -> String -> List Expression -> Expression -> Html LaTeXMsg
renderEnvironment selectedId state name args body =
    case Dict.get name renderEnvironmentDict of
        Just f ->
            f selectedId state args body

        Nothing ->
            renderDefaultEnvironment selectedId state name args body


renderEnvironmentDict : Dict.Dict String (String -> LaTeXState -> List Expression -> Expression -> Html LaTeXMsg)
renderEnvironmentDict =
    -- s x a y = SourceText, LaTeXState, List LatexExpression, LaTeXExpression
    -- s l e = LaTexState, List Expression, Expression
    Dict.fromList
        [ ( "align", \si s l e -> renderMathEnvironment "aligned" si s l e )
        , ( "matrix", \si s l e -> renderMathEnvironment "matrix" si s l e )
        , ( "pmatrix", \si s l e -> renderMathEnvironment "pmatrix" si s l e )
        , ( "bmatrix", \si s l e -> renderMathEnvironment "bmatrix" si s l e )
        , ( "Bmatrix", \si s l e -> renderMathEnvironment "Bmatrix" si s l e )
        , ( "vmatrix", \si s l e -> renderMathEnvironment "vmatrix" si s l e )
        , ( "Vmatrix", \si s l e -> renderMathEnvironment "Vmatrix" si s l e )

        --, ( "colored", \s l e -> renderCodeEnvironment s l e )
        --, ( "center", \s l e -> renderCenterEnvironment s l e )
        --, ( "obeylines", \s l e -> renderObeyLinesEnvironment s l e )
        --, ( "CD", \s l e -> renderMathJaxEnvironment "CD" s l e )
        --, ( "comment", \s l e -> renderCommentEnvironment s l e )
        --, ( "defitem", \s l e -> renderDefItemEnvironment s l e )
        --, ( "enumerate", \s l e -> renderEnumerate s l e )
        --, ( "eqnarray", \s l e -> renderEqnArray s l e )
        , ( "equation", \si s _ e -> renderEquationEnvironment si s e )
        , ( "equation", \si s _ e -> renderEquationEnvironment si s e )

        --, ( "indent", \s l e -> renderIndentEnvironment s l e )
        --, ( "itemize", \s l e -> renderItemize s l e )
        --, ( "listing", \s l e -> renderListing s l e )
        --, ( "macros", \s l e -> renderMacros s l e )
        --, ( "maskforweb", \s l e -> renderCommentEnvironment s l e )
        --, ( "quotation", \s l e -> renderQuotation s l e )
        --, ( "tabular", \s l e -> renderTabular s l e )
        --, ( "thebibliography", \s l e -> renderTheBibliography s l e )
        --, ( "useforweb", \s l e -> renderUseForWeb s l e )
        --, ( "verbatim", \s l e -> renderVerbatim s l e )
        --, ( "verse", \s l e -> renderVerse s l e )
        --, ( "mathmacro", \s l e -> renderMathMacros s l e )
        --, ( "textmacro", \s l e -> renderTextMacros s l e )
        --, ( "svg", \s l e -> renderSvg s l e )
        ]


theoremLikeEnvironments : List String
theoremLikeEnvironments =
    [ "theorem"
    , "proposition"
    , "corollary"
    , "lemma"
    , "definition"
    , "problem"
    ]


renderDefaultEnvironment : String -> LaTeXState -> String -> List Expression -> Expression -> Html LaTeXMsg
renderDefaultEnvironment selectedId state name args body =
    if List.member name theoremLikeEnvironments then
        renderTheoremLikeEnvironment selectedId state name args body

    else
        renderDefaultEnvironment2 selectedId state (Utility.capitalize name) args body


renderTheoremLikeEnvironment : String -> LaTeXState -> String -> List Expression -> Expression -> Html LaTeXMsg
renderTheoremLikeEnvironment selectedId latexState name args body =
    let
        r =
            render selectedId latexState [ body ]

        eqno =
            LaTeXState.getCounter "eqno" latexState

        s1 =
            LaTeXState.getCounter "s1" latexState

        tno =
            LaTeXState.getCounter "tno" latexState

        tnoString =
            if s1 > 0 then
                " " ++ String.fromInt s1 ++ "." ++ String.fromInt tno

            else
                " " ++ String.fromInt tno

        sm =
            Parser.Expression.getSource body
    in
    Html.div [ HA.class "environment" ]
        [ Html.strong [] [ Html.text (Utility.capitalize name ++ tnoString) ]
        , Html.div [ HA.class "italic" ] r
        ]


renderDefaultEnvironment2 : String -> LaTeXState -> String -> List Expression -> Expression -> Html LaTeXMsg
renderDefaultEnvironment2 selectedId latexState name args body =
    let
        r =
            render selectedId latexState [ body ]

        sm =
            Parser.Expression.getSource body
    in
    Html.div (active sm selectedId)
        [ Html.strong [] [ Html.text name ]
        , Html.div [] r
        ]



-- RENDER INDIVIDUAL ENVIRONMENTS
--renderSvg : SourceText -> LatexState -> LatexExpression -> Html msg
--renderSvg source latexState body =
--    case SvgParser.parse (Internal.RenderToString.render latexState body) of
--        Ok html_ ->
--            html_
--
--        Err _ ->
--            Html.span [ HA.class "X6" ] [ Html.text "SVG parse error" ]


renderMathEnvironment : String -> String -> LaTeXState -> List Expression -> Expression -> Html LaTeXMsg
renderMathEnvironment selectedId envName latexState _ body =
    let
        --r =
        --    Internal.RenderToString.render latexState body
        eqno =
            LaTeXState.getCounter "eqno" latexState

        s1 =
            LaTeXState.getCounter "s1" latexState

        addendum =
            if eqno > 0 then
                if s1 > 0 then
                    "\\tag{" ++ String.fromInt s1 ++ "." ++ String.fromInt eqno ++ "}"

                else
                    "\\tag{" ++ String.fromInt eqno ++ "}"

            else
                ""

        -- (innerContents, sourceMap) : (String, SourceMap)
        ( innerContents, sourceMap ) =
            case body of
                Text str sm ->
                    ( str
                        |> String.trim
                        --|> Internal.MathMacro.evalStr latexState.mathMacroDictionary
                        -- TODO: implement macro expansion
                        |> String.replace "\\ \\" "\\\\"
                        |> Parser.Helpers.removeLabel
                    , sm
                    )

                _ ->
                    ( "", Parser.Expression.dummySourceMap )

        --  "Parser error in render align environment"
        content =
            -- REVIEW: changed for KaTeX
            "\n\\begin{" ++ envName ++ "}\n" ++ innerContents ++ "\n\\end{" ++ envName ++ "}\n"

        tag =
            case Parser.Helpers.getTag addendum of
                Nothing ->
                    ""

                Just tag_ ->
                    "(" ++ tag_ ++ ")"
    in
    displayMathTextWithLabel_ selectedId latexState sourceMap content tag


renderEquationEnvironment : String -> LaTeXState -> Expression -> Html LaTeXMsg
renderEquationEnvironment selectedId latexState body =
    let
        eqno =
            LaTeXState.getCounter "eqno" latexState

        s1 =
            LaTeXState.getCounter "s1" latexState

        addendum =
            if eqno > 0 then
                if s1 > 0 then
                    "\\tag{" ++ String.fromInt s1 ++ "." ++ String.fromInt eqno ++ "}"

                else
                    "\\tag{" ++ String.fromInt eqno ++ "}"

            else
                ""

        ( contents, sm ) =
            case body of
                Text str sm_ ->
                    ( str
                        |> String.trim
                        --|> Internal.MathMacro.evalStr latexState.mathMacroDictionary
                        -- TODO: implement macro expansion
                        |> Parser.Helpers.removeLabel
                    , sm_
                    )

                _ ->
                    ( "Parser error in render equation environment", Parser.Expression.dummySourceMap )

        tag =
            case Parser.Helpers.getTag addendum of
                Nothing ->
                    ""

                Just tag_ ->
                    --   "\\qquad (" ++ tag_ ++ ")"
                    "(" ++ tag_ ++ ")"
    in
    -- ("\\begin{equation}" ++ contents ++ addendum ++ "\\end{equation}")
    -- REVIEW; changed for KaTeX
    -- displayMathText_ latexState  (contents ++ tag)
    displayMathTextWithLabel_ selectedId latexState sm contents tag


displayMathTextWithLabel_ : String -> LaTeXState -> SourceMap -> String -> String -> Html LaTeXMsg
displayMathTextWithLabel_ selectedId latexState sm str label =
    Html.div
        []
        [ Html.div ([ HA.style "float" "right", HA.style "margin-top" "3px" ] ++ active sm selectedId)
            [ Html.text label ]
        , Html.div (active sm selectedId)
            [ mathText DisplayMathMode selectedId (String.trim str) sm ]
        ]



--mathText : DisplayMode -> String -> SourceMap -> Html LaTeXMsg
--mathText displayMode content sm
--
--
--displayMathJaxTextWithLabel_ : LaTeXState -> String -> String -> Html msg
--displayMathJaxTextWithLabel_ latexState str label =
--    Html.div
--        []
--        [ Html.div [ HA.style "float" "right", HA.style "margin-top" "3px" ]
--            [ Html.text label ]
--        , Html.div []
--            -- [ Html.text "MathJax"]
--            [ mathJaxText DisplayMathMode (String.trim str) ]
--        ]
--
--
--renderMathJaxEnvironment : EnvName -> SourceText -> LatexState -> LatexExpression -> Html msg
--renderMathJaxEnvironment envName source latexState body =
--    let
--        r =
--            Internal.RenderToString.render latexState body
--
--        eqno =
--            LaTeXState.getCounter "eqno" latexState
--
--        s1 =
--            LaTeXState.getCounter "s1" latexState
--
--        addendum =
--            if eqno > 0 then
--                if s1 > 0 then
--                    "\\tag{" ++ String.fromInt s1 ++ "." ++ String.fromInt eqno ++ "}"
--
--                else
--                    "\\tag{" ++ String.fromInt eqno ++ "}"
--
--            else
--                ""
--
--        innerContents =
--            case body of
--                LXString str ->
--                    str
--                        |> String.trim
--                        |> Internal.MathMacro.evalStr latexState.mathMacroDictionary
--                        |> String.replace "\\ \\" "\\\\"
--                        |> Internal.ParserHelpers.removeLabel
--                        |> (\x -> "\\begin{" ++ envName ++ "}\n" ++ x ++ "\n\\end{" ++ envName ++ "}")
--
--                _ ->
--                    ""
--
--        --  "Parser error in render align environment"
--        content =
--            -- REVIEW: changed for KaTeX
--            "\n\\begin{" ++ envName ++ "}\n" ++ innerContents ++ "\n\\end{" ++ envName ++ "}\n"
--
--        tag =
--            case Internal.ParserHelpers.getTag addendum of
--                Nothing ->
--                    ""
--
--                Just tag_ ->
--                    "(" ++ tag_ ++ ")"
--    in
--    displayMathJaxTextWithLabel_ latexState innerContents tag
--
--
--renderCenterEnvironment : SourceText -> LatexState -> LatexExpression -> Html msg
--renderCenterEnvironment source latexState body =
--    let
--        r =
--            render source latexState body
--    in
--    Html.div
--        [ HA.style "display" "flex"
--        , HA.style "flex-direction" "row"
--        , HA.style "justify-content" "center"
--        ]
--        [ r ]
--
--
--renderObeyLinesEnvironment : SourceText -> LatexState -> LatexExpression -> Html msg
--renderObeyLinesEnvironment source latexState body =
--    let
--        r =
--            render source latexState body
--    in
--    Html.div
--        [ HA.style "white-space" "pre"
--        ]
--        [ r ]
--
--
--renderCommentEnvironment : SourceText -> LatexState -> LatexExpression -> Html msg
--renderCommentEnvironment source latexState body =
--    Html.div [] []
--
--
--renderEnumerate : SourceText -> LatexState -> LatexExpression -> Html msg
--renderEnumerate source latexState body =
--    -- TODO: fix spacing issue
--    Html.ol [ HA.style "margin-top" "0px" ] [ render source latexState body ]
--
--
--renderDefItemEnvironment : SourceText -> LatexState -> List LatexExpression -> LatexExpression -> Html msg
--renderDefItemEnvironment source latexState optArgs body =
--    Html.div []
--        [ Html.strong [] [ Html.text <| Internal.RenderToString.renderArg 0 latexState optArgs ]
--        , Html.div [ HA.style "margin-left" "25px", HA.style "margin-top" "10px" ] [ render source latexState body ]
--        ]
--
--
--{-| XXX
---}
--renderEqnArray : SourceText -> LatexState -> LatexExpression -> Html msg
--renderEqnArray source latexState body =
--    let
--        body1 =
--            Internal.RenderToString.render latexState body
--
--        body2 =
--            -- REVIEW: changed for KaTeX
--            "\\begin{aligned}" ++ body1 ++ "\\end{aligned}"
--    in
--    displayMathText latexState body2
--
--
--renderEquationEnvironment : SourceText
--
--renderIndentEnvironment : SourceText -> LatexState -> LatexExpression -> Html msg
--renderIndentEnvironment source latexState body =
--    Html.div [ HA.style "margin-left" "2em" ] [ render source latexState body ]
--
--
--renderItemize : SourceText -> LatexState -> LatexExpression -> Html msg
--renderItemize source latexState body =
--    -- TODO: fix space issue
--    Html.ul [ HA.style "margin-top" "0px" ] [ render source latexState body ]
--
--
--renderListing : SourceText -> LatexState -> LatexExpression -> Html msg
--renderListing _ latexState body =
--    let
--        text =
--            Internal.RenderToString.render latexState body
--
--        lines =
--            Utility.addLineNumbers text
--    in
--    Html.pre [ HA.class "verbatim" ] [ Html.text lines ]
--
-- END: RENDER ENVIRONMENT


mathText : DisplayMode -> String -> String -> SourceMap -> Html LaTeXMsg
mathText displayMode selectedId content sm =
    Html.node "math-text"
        (active sm selectedId
            ++ [ HA.property "delay" (Json.Encode.bool False)
               , HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
               , HA.property "content" (Json.Encode.string (content |> String.replace "\\ \\" "\\\\"))
               , clicker sm
               , HA.id (id sm)

               --, HA.property "content" (Json.Encode.string content |> String.replace "\\ \\" "\\\\"))
               ]
        )
        []


isDisplayMathMode : DisplayMode -> Bool
isDisplayMathMode displayMode =
    case displayMode of
        InlineMathMode ->
            False

        DisplayMathMode ->
            True


inlineMathText : String -> String -> SourceMap -> Html LaTeXMsg
inlineMathText selectedId str_ sm =
    mathText InlineMathMode selectedId (String.trim str_) sm


displayMathText : String -> String -> SourceMap -> Html LaTeXMsg
displayMathText selectedId str_ sm =
    mathText DisplayMathMode selectedId (String.trim str_) sm


at : Int -> String -> Maybe String
at k str =
    String.lines str |> List.Extra.getAt k


highlightWithSourceMap : Parser.Expression.SourceMap -> String -> List (List Int) -> Html msg
highlightWithSourceMap sm str sourceMapIndex_ =
    let
        selection =
            Parser.Expression.getSelectionFromSourceMap sm str sourceMapIndex_

        idxs =
            String.indices selection str
    in
    case List.head idxs of
        Nothing ->
            Html.span [ HA.id (id sm) ] [ Html.text str ]

        Just offset ->
            let
                left =
                    String.left offset str

                remainder =
                    String.dropLeft offset str

                middle =
                    String.left sm.length remainder

                right =
                    String.dropLeft sm.length remainder
            in
            Html.div [ HA.id (id sm) ]
                [ Html.span [] [ Html.text left ]
                , Html.span [ HA.style "background-color" "pink" ] [ Html.text middle ]
                , Html.span [] [ Html.text right ]
                ]



-- MACRO DICT


type alias MacroDict =
    Dict String (LaTeXState -> Maybe String -> List Expression -> SourceMap -> Html LaTeXMsg)



--macroDict : MacroDict


macroDict =
    Dict.fromList
        [ ( "strong", \si state ms args sm -> render si state args |> Html.span (HA.style "font-weight" "bold" :: active sm si) )
        , ( "italic", \si state ms args sm -> render si state args |> Html.span (HA.style "font-style" "italic" :: active sm si) )
        , ( "red", \si state ms args sm -> render si state args |> Html.span (HA.style "color" state.config.redColor :: active sm si) )
        , ( "blue", \si state ms args sm -> render si state args |> Html.span (HA.style "font-style" "italic" :: active sm si) )
        ]
