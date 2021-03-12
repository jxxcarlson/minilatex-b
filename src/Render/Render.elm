module Render.Render exposing (render, highlightWithSourceMap)

{-| The render function transforma an Expression to Html,
given a LaTeXState, which we _assume to have already been ocomputed._

@docs render, highlightWithSourceMap

-}

import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Json.Encode
import LaTeXMsg exposing (LaTeXMsg(..))
import List.Extra
import Parser.Expression exposing (Expression(..), SourceMap)
import Parser.Helpers
import Render.Image
import Render.LaTeXState as LaTeXState exposing (LaTeXState)
import Utility


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


{-| render a list of Expression using a given selectedId and LaTeXState.
The selectedId determines wich element in the renderedText (if any) is
highlighted. The LaTeXState carries information on cross-references,
section numbers, etc.
-}
render : String -> LaTeXState -> List Expression -> List (Html LaTeXMsg)
render selectedId state exprs =
    List.map (renderExpr selectedId state) exprs


renderToStingList : List Expression -> List String
renderToStingList exprs =
    List.map Parser.Expression.toString exprs


getStringAtWithDefault : Int -> String -> List String -> String
getStringAtWithDefault k default strings =
    List.Extra.getAt k strings |> Maybe.withDefault default



-- |> List.map Parser.Expression.toString


clicker sm =
    onClick (SendSourceMap sm)


makeId sm =
    String.fromInt sm.generation ++ ":" ++ String.fromInt sm.blockOffset ++ "-" ++ String.fromInt sm.offset


idSuffix : String -> String
idSuffix str =
    String.split ":" str
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault "nada"


active : SourceMap -> String -> List (Attribute LaTeXMsg)
active sm selectedId =
    let
        id_ =
            makeId sm
    in
    [ clicker sm, HA.id id_, highlight "#FAA" selectedId id_ ]


highlight : String -> String -> String -> Attribute LaTeXMsg
highlight color selectedId id_ =
    if idSuffix id_ == selectedId then
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

        NewCommand _ _ _ _ ->
            Html.span [] []

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
errorString p _ =
    " << "
        ++ Parser.Expression.problemAsString p


macro : String -> LaTeXState -> String -> Maybe Expression -> List Expression -> SourceMap -> Html LaTeXMsg
macro selectedId state name optArg args sm =
    case Dict.get name macroDict of
        Nothing ->
            undefinedMacro name sm

        Just f ->
            f selectedId state optArg args sm


undefinedMacro : String -> SourceMap -> Html LaTeXMsg
undefinedMacro name sm =
    Html.span [ HA.id (makeId sm), clicker sm, HA.style "color" "red" ] [ Html.text <| "Undefined macro: " ++ name ]


environment selectedId state name args body _ =
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
               , HA.id (makeId sm)

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


{-| Highlight the segment of a text defined by the sourceMap. The information
in the source map is used to split the full text into left, middle and right
parts, where the middle part is the given text. The three parts are then
reassembled as html list values, with the middle part highlighted.
-}
highlightWithSourceMap : Parser.Expression.SourceMap -> String -> List (List Int) -> Html msg
highlightWithSourceMap sourceMap text sourceMapIndex_ =
    let
        selection : String
        selection =
            Parser.Expression.getSelectionFromSourceMap sourceMap text sourceMapIndex_

        idxs : List Int
        idxs =
            String.indices selection text
    in
    case List.head idxs of
        Nothing ->
            Html.span [ HA.id (makeId sourceMap) ] [ Html.text text ]

        Just offset ->
            let
                left =
                    String.left offset text

                remainder =
                    String.dropLeft offset text

                middle =
                    String.left sourceMap.length remainder

                right =
                    String.dropLeft sourceMap.length remainder
            in
            Html.div [ HA.id (makeId sourceMap) ]
                [ Html.span [] [ Html.text left ]
                , Html.span [ HA.style "background-color" "pink" ] [ Html.text middle ]
                , Html.span [] [ Html.text right ]
                ]


type alias MacroDict =
    Dict String (LaTeXState -> Maybe String -> List Expression -> SourceMap -> Html LaTeXMsg)



-- MACRODICT


macroDict =
    Dict.fromList
        [ ( "strong", \si state ms args sm -> rmas si ms state args sm [ HA.style "font-weight" "bold" ] )
        , ( "textbf", \si state ms args sm -> rmas si ms state args sm [ HA.style "font-weight" "bold" ] )
        , ( "italic", \si state ms args sm -> rmas si ms state args sm [ HA.style "font-style" "italic" ] )
        , ( "term", \si state ms args sm -> rmas si ms state args sm [ HA.style "font-style" "italic" ] )
        , ( "emph", \si state ms args sm -> rmas si ms state args sm [ HA.style "font-style" "italic" ] )
        , ( "par", \si state ms args sm -> rmas si ms state args sm [ HA.style "height" "10px" ] )
        , ( "par", \si state ms args sm -> rmas si ms state args sm [ HA.style "height" "10px" ] )
        , ( "bigskip", \si state ms args sm -> rmas si ms state args sm [ HA.style "height" "40px" ] )
        , ( "medskip", \si state ms args sm -> rmas si ms state args sm [ HA.style "height" "10px" ] )
        , ( "smallksip", \si state ms args sm -> rmas si ms state args sm [ HA.style "height" "0px" ] )
        , ( "red", \si state ms args sm -> rmas si ms state args sm [ HA.style "color" state.config.redColor ] )
        , ( "blue", \si state ms args sm -> rmas si ms state args sm [ HA.style "color" state.config.blueColor ] )
        , ( "highlight", \si state ms args sm -> rmas si ms state args sm [ HA.style "background-color" "yellow !important", HA.style "padding" "3px" ] ) -- TODO: not working
        , ( "strike", \si state ms args sm -> rmas si ms state args sm [ HA.style "text-decoration" "line-through" ] )
        , ( "code", \si state ms args sm -> rmas si ms state args sm [ HA.style "font-family" "Monospace", HA.style "color" state.config.redColor ] )
        , ( "section", \si state ms args sm -> renderSection sectionNumber si ms state args sm [ HA.style "font-size" "160%" ] )
        , ( "subsection", \si state ms args sm -> renderSection subsectionNumber si ms state args sm [ HA.style "font-size" "130%" ] )
        , ( "subsubsection", \si state ms args sm -> renderSection subsubsectionNumber si ms state args sm [ HA.style "font-size" "110%" ] )
        , ( "section*", \si state ms args sm -> renderSection noSectionNumber si ms state args sm [ HA.style "font-size" "150%" ] )
        , ( "subsection*", \si state ms args sm -> renderSection noSectionNumber si ms state args sm [ HA.style "font-size" "125%" ] )
        , ( "subsubsection*", \si state ms args sm -> renderSection noSectionNumber si ms state args sm [ HA.style "font-size" "100%" ] )
        , ( "href", \si state ms args sm -> renderHRef state args )
        , ( "image", \si state ms args sm -> renderImage state args )
        , ( "include", \si state ms args sm -> nullSpan )
        , ( "setclient", \si state ms args sm -> nullSpan )
        , ( "setdocid", \si state ms args sm -> nullSpan )
        , ( "title", \si state ms args sm -> nullSpan )
        , ( "author", \si state ms args sm -> nullSpan )
        , ( "date", \si state ms args sm -> nullSpan )
        , ( "revision", \si state ms args sm -> nullSpan )
        , ( "index", \si state ms args sm -> nullSpan )
        , ( "ilink1", \si state ms args sm -> nullSpan )
        , ( "ilink2", \si state ms args sm -> nullSpan )
        , ( "ilink3", \si state ms args sm -> nullSpan )
        , ( "xlink", \si state ms args sm -> renderXLink Nothing state args )
        , ( "publiclink", \si state ms args sm -> renderXLink Nothing state args )
        , ( "homepagelink", \si state ms args sm -> renderXLink (Just "h") state args )
        , ( "note", \si state ms args sm -> attachNote args )
        , ( "cite", \si state ms args sm -> renderCite state args )
        , ( "dollar", \si state ms args sm -> Html.span [] [ Html.text "$" ] )
        , ( "percent", \si state ms args sm -> Html.span [] [ Html.text "%" ] )
        , ( "texbegin", \si state ms args sm -> Html.span [] [ Html.text "\\begin" ] )
        , ( "texend", \si state ms args sm -> Html.span [] [ Html.text "\\end" ] )
        , ( "underscore", \si state ms args sm -> Html.span [] [ Html.text "_" ] )
        , ( "mdash", \si state ms args sm -> Html.span [] [ Html.text "— " ] )
        , ( "ndash", \si state ms args sm -> Html.span [] [ Html.text "– " ] )
        , ( "eqref", \si state ms args sm -> renderEqRef state args )
        , ( "ref", \si state ms args sm -> renderRef state args )
        ]



-- NEW


renderRef : LaTeXState -> List Expression -> Html msg
renderRef latexState args =
    let
        args_ =
            renderToStingList args

        key =
            getStringAtWithDefault 0 "KEY" args_
    in
    Html.span [] [ Html.text <| LaTeXState.getCrossReference key latexState ]


renderEqRef : LaTeXState -> List Expression -> Html msg
renderEqRef latexState args =
    let
        args_ =
            renderToStingList args

        key =
            getStringAtWithDefault 0 "KEY" args_

        ref =
            LaTeXState.getCrossReference key latexState
    in
    Html.i [] [ Html.text "(", Html.text ref, Html.text ")" ]


renderCite : LaTeXState -> List Expression -> Html msg
renderCite latexState args =
    let
        args_ =
            renderToStingList args

        label_ =
            getStringAtWithDefault 0 "LABLE" args_

        ref =
            LaTeXState.getDictionaryItem ("bibitem:" ++ label_) latexState

        label =
            if ref /= "" then
                ref

            else
                label_
    in
    Html.strong []
        [ Html.span [] [ Html.text "[" ]
        , Html.a [ HA.href ("#bibitem:" ++ label) ] [ Html.text label ]
        , Html.span [] [ Html.text "] " ]
        ]


attachNote : List Expression -> Html msg
attachNote args =
    -- TODO: Finish this
    let
        args_ =
            renderToStingList args

        author =
            getStringAtWithDefault 1 "AUTHOR" args_

        content =
            getStringAtWithDefault 0 "CONTENT" args_
    in
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ Html.div [ HA.style "background-color" "yellow", HA.style "padding" "8px" ]
            [ Html.text <| content
            , Html.div [ HA.style "font-weight" "bold" ] [ Html.text <| author ]
            ]
        ]


nullSpan =
    Html.span [] []



-- Utility


{-| rmas = render macro as span
-}
rmas : String -> b -> LaTeXState -> List Expression -> SourceMap -> List (Attribute LaTeXMsg) -> Html LaTeXMsg
rmas si ms state args sm st =
    render si state args |> Html.span (st ++ active sm si)


renderHRef : LaTeXState -> List Expression -> Html msg
renderHRef latexState args =
    let
        args_ =
            renderToStingList args

        url =
            getStringAtWithDefault 0 "URL" args_

        label =
            getStringAtWithDefault 1 "LABEL" args_
    in
    Html.a [ HA.href url, HA.target "_blank" ] [ Html.text label ]



-- LINK


renderXLink : Maybe String -> LaTeXState -> List Expression -> Html msg
renderXLink urlFragment latexState args =
    -- e.g, let urlFragment = "h" for homePageLink
    let
        args_ =
            renderToStingList args

        id =
            getStringAtWithDefault 0 "nada" args_

        ref =
            case urlFragment of
                Nothing ->
                    LaTeXState.getDictionaryItem "setclient" latexState ++ "/" ++ id

                Just frag ->
                    LaTeXState.getDictionaryItem "setclient" latexState ++ "/" ++ frag ++ "/" ++ id

        label =
            getStringAtWithDefault 1 "nada" args_
    in
    Html.a [ HA.href ref ] [ Html.text label ]



-- SECTION


renderSection : (LaTeXState -> Html LaTeXMsg) -> String -> b -> LaTeXState -> List Expression -> SourceMap -> List (Attribute LaTeXMsg) -> Html LaTeXMsg
renderSection labelFunction si ms state args sm st =
    labelFunction state :: render si state args |> Html.span (st ++ active sm si)


sectionNumber : LaTeXState -> Html msg
sectionNumber state =
    Html.span [] [ Html.text <| String.fromInt (LaTeXState.getCounter "s1" state) ++ ". " ]


subsectionNumber : LaTeXState -> Html msg
subsectionNumber state =
    Html.span [] [ Html.text <| String.fromInt (LaTeXState.getCounter "s1" state) ++ "." ++ String.fromInt (LaTeXState.getCounter "s2" state) ++ ". " ]


subsubsectionNumber : LaTeXState -> Html msg
subsubsectionNumber state =
    Html.span [] [ Html.text <| String.fromInt (LaTeXState.getCounter "s1" state) ++ "." ++ String.fromInt (LaTeXState.getCounter "s2" state) ++ "." ++ String.fromInt (LaTeXState.getCounter "s3" state) ++ ". " ]


noSectionNumber : LaTeXState -> Html msg
noSectionNumber state =
    Html.span [] []



-- IMAGE


renderImage : LaTeXState -> List Expression -> Html msg
renderImage latexState args =
    let
        args_ =
            renderToStingList args

        url =
            getStringAtWithDefault 0 "URL" args_

        label =
            getStringAtWithDefault 1 "LABEL" args_

        attributeString =
            getStringAtWithDefault 2 "IMAGE ATTRIBUTES" args_

        imageAttrs =
            Render.Image.parseImageAttributes attributeString

        width =
            String.fromInt imageAttrs.width ++ "px"
    in
    if imageAttrs.float == "left" then
        Html.div [ HA.style "float" "left" ]
            [ Html.img [ HA.src url, HA.alt label, HA.style "width" width, HA.style "margin-right" "12px" ] []
            , Html.br [] []
            , Html.div [ HA.style "width" width, HA.style "text-align" "center", HA.style "display" "block" ] [ Html.text label ]
            ]

    else if imageAttrs.float == "right" then
        Html.div [ HA.style "float" "right" ]
            [ Html.img [ HA.src url, HA.alt label, HA.style "width" width, HA.style "margin-left" "12px" ] []
            , Html.br [] []
            , Html.div [ HA.style "width" width, HA.style "text-align" "center", HA.style "display" "block" ] [ Html.text label ]
            ]

    else if imageAttrs.align == "center" then
        Html.div [ HA.style "margin-left" "auto", HA.style "margin-right" "auto", HA.style "width" width ]
            [ Html.img [ HA.src url, HA.alt label, HA.style "width" width ] []
            , Html.br [] []
            , Html.div [ HA.style "width" width, HA.style "text-align" "center", HA.style "display" "block" ] [ Html.text label ]
            ]

    else
        Html.div [ HA.style "margin-left" "auto", HA.style "margin-right" "auto", HA.style "width" width ]
            [ Html.img [ HA.src url, HA.alt label, HA.style "width" width ] []
            , Html.br [] []
            , Html.div [ HA.style "width" width, HA.style "text-align" "center", HA.style "display" "block" ] [ Html.text label ]
            ]


renderImageRef : LaTeXState -> List Expression -> Html msg
renderImageRef latexState args =
    let
        args_ =
            renderToStingList args

        url =
            getStringAtWithDefault 0 "URL" args_

        imageUrl =
            getStringAtWithDefault 1 "IMAGE URL" args_

        attributeString =
            getStringAtWithDefault 2 "IMAGE ATTRIBUTES" args_

        imageAttrs =
            Render.Image.parseImageAttributes attributeString

        width =
            String.fromInt imageAttrs.width ++ "px"

        theImage =
            if imageAttrs.float == "left" then
                Html.div [ HA.style "float" "left" ]
                    [ Html.img [ HA.src imageUrl, HA.alt "image link", HA.style "width" width, HA.style "margin-right" "12px" ] []
                    , Html.br [] []
                    , Html.div [ HA.style "width" width, HA.style "text-align" "center", HA.style "display" "block" ] []
                    ]

            else if imageAttrs.float == "right" then
                Html.div [ HA.style "float" "right" ]
                    [ Html.img [ HA.src imageUrl, HA.alt "image link", HA.style "width" width, HA.style "margin-left" "12px" ] []
                    , Html.br [] []
                    , Html.div [ HA.style "width" width, HA.style "text-align" "center", HA.style "display" "block" ] []
                    ]

            else if imageAttrs.align == "center" then
                Html.div [ HA.style "margin-left" "auto", HA.style "margin-right" "auto", HA.style "width" width ]
                    [ Html.img [ HA.src imageUrl, HA.alt "image link", HA.style "width" width ] []
                    , Html.br [] []
                    , Html.div [ HA.style "width" width, HA.style "text-align" "center", HA.style "display" "block" ] []
                    ]

            else
                Html.div [ HA.style "margin-left" "auto", HA.style "margin-right" "auto", HA.style "width" width ]
                    [ Html.img [ HA.src imageUrl, HA.alt "image link", HA.style "width" width ] []
                    , Html.br [] []
                    , Html.div [ HA.style "width" width, HA.style "text-align" "center", HA.style "display" "block" ] []
                    ]
    in
    Html.a [ HA.href url ] [ theImage ]


{-| rmas = render macro as code
-}
rmac si ms state args sm st =
    render si state args |> Html.code (st ++ active sm si)



-- OLD
--[
--       , ( "colored", \s x y z -> renderColored s x z )
--       , ( "percent", \s x y z -> renderPercent s x z )
--       , ( "ellie", \s x y z -> renderEllie s x z )
--       , ( "iframe", \s x y z -> renderIFrame s x z )
--       , ( "label", \s x y z -> renderLabel s x z )
--       , ( "maintableofcontents", \s x y z -> renderMainTableOfContents s x z )
--       , ( "maketitle", \s x y z -> renderMakeTitle s x z )
--       , ( "bs", \s x y z -> renderBackslash s x z )
--       , ( "texarg", \s x y z -> renderTexArg s x z )
--       , ( "setcounter", \s x y z -> renderSetCounter s x z )
--       , ( "subheading", \s x y z -> renderSubheading s x z )
--       , ( "tableofcontents", \s x y z -> renderTableOfContents s x z )
--       , ( "innertableofcontents", \s x y z -> renderInnerTableOfContents s x z )
--       , ( "remote", \s x y z -> renderRemote s x z )
--       , ( "local", \s x y z -> renderLocal s x z )
--       , ( "documentTitle", \s x y z -> renderDocumentTitle s x z )
--       , ( "email", \s x y z -> renderEmail s x z )
--       , ( "uuid", \s x y z -> renderUuid s x z )
--       ]
