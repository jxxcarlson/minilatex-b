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
import Parser exposing (DeadEnd, Problem(..))
import Parser.Expression exposing (Expression(..), SourceMap)
import Parser.Helpers
import Parser.Parser as Parser
import Regex
import Render.Image
import Render.LaTeXState as LaTeXState exposing (LaTeXState)
import SyntaxHighlight
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


{-| render 0-th arg to string
-}
renderArg : List Expression -> String
renderArg expressions =
    List.Extra.getAt 0 (List.map Parser.Expression.toString expressions) |> Maybe.withDefault "ARG"



-- |> List.map Parser.Expression.toString


clicker sm =
    onClick (SendSourceMap sm)


makeId sm =
    String.fromInt sm.generation ++ ":" ++ String.fromInt sm.blockOffset ++ "-" ++ String.fromInt sm.offset


makeId_ : String -> String -> String
makeId_ prefix name =
    String.join "_" [ "", prefix, compress "_" name ]


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
        , ( "mathmacro", \si s l e -> Html.div [] [] )
        , ( "textmacro", \si s l e -> Html.div [] [] )

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
        , ( "subheading", \si state ms args sm -> rmad si ms state args sm [ HA.style "font-weight" "bold" ] )
        , ( "remote", \si state ms args sm -> rmad si ms state args sm [ HA.style "color" "red", HA.style "white-space" "pre" ] )
        , ( "local", \si state ms args sm -> rmad si ms state args sm [ HA.style "color" "blue", HA.style "white-space" "pre" ] )
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
        , ( "uuid", \si state ms args sm -> nullSpan )
        , ( "email", \si state ms args sm -> nullSpan )
        , ( "label", \si state ms args sm -> nullSpan )
        , ( "maintableofcontents", \si state ms args sm -> nullSpan )
        , ( "setcounter", \si state ms args sm -> nullSpan )
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
        , ( "bs", \si state ms args sm -> Html.span [] [ Html.text <| "\\" ++ renderArg args ] )
        , ( "texarg", \si state ms args sm -> Html.text <| "{" ++ renderArg args ++ "}" )
        , ( "maketitle", \si state ms args sm -> maketitle state )
        , ( "tableofcontents", \si state ms args sm -> tableOfContents state )
        , ( "innertableofcontents", \si state ms args sm -> innerTableOfContents state )
        , ( "colored", \si state ms args sm -> colored state args )
        , ( "ellie", \si state ms args sm -> ellie args )
        , ( "iframe", \si state ms args sm -> iframe args )
        ]



-- NEW
-- TABLE OF CONTENTS


tableOfContents : LaTeXState -> Html msg
tableOfContents latexState =
    let
        innerPart =
            makeTableOfContents latexState
    in
    Html.div []
        [ Html.h3 [] [ Html.text "Table of Contents" ]
        , Html.ul [] innerPart
        ]


innerTableOfContents : LaTeXState -> Html msg
innerTableOfContents latexState =
    let
        s1 =
            LaTeXState.getCounter "s1" latexState

        prefix =
            String.fromInt s1 ++ "."

        innerPart =
            makeInnerTableOfContents prefix latexState
    in
    Html.div []
        [ Html.h3 [] [ Html.text "Table of Contents" ]
        , Html.ul [] innerPart
        ]


{-| Build a table of contents from the
current LatexState; use only level 1 items
-}
makeTableOfContents : LaTeXState -> List (Html msg)
makeTableOfContents latexState =
    let
        toc =
            List.filter (\item -> item.level == 1) latexState.tableOfContents
    in
    List.foldl (\tocItem acc -> acc ++ [ makeTocItem "" tocItem ]) [] (List.indexedMap Tuple.pair toc)


{-| Build a table of contents from the
current LatexState; use only level 2 items
-}
makeInnerTableOfContents : String -> LaTeXState -> List (Html msg)
makeInnerTableOfContents prefix latexState =
    let
        toc =
            List.filter (\item -> item.level == 2) latexState.tableOfContents
    in
    List.foldl (\tocItem acc -> acc ++ [ makeTocItem prefix tocItem ]) [] (List.indexedMap Tuple.pair toc)


makeTocItem : String -> ( Int, LaTeXState.TocEntry ) -> Html msg
makeTocItem prefix tocItem =
    let
        i =
            Tuple.first tocItem

        ti =
            Tuple.second tocItem

        number =
            prefix ++ String.fromInt (i + 1) ++ ". "

        classProperty =
            "class=\"sectionLevel" ++ String.fromInt ti.level ++ "\""

        id =
            makeIdWithPrefix (sectionPrefix ti.level) ti.name

        href =
            "#" ++ id
    in
    Html.p
        [ HA.style "font-size" "14px"
        , HA.style "padding-bottom" "0px"
        , HA.style "margin-bottom" "0px"
        , HA.style "padding-top" "0px"
        , HA.style "margin-top" "0px"
        , HA.style "line-height" "20px"
        ]
        [ Html.text number
        , Html.a [ HA.href href ] [ Html.text ti.name ]
        ]


makeIdWithPrefix : String -> String -> String
makeIdWithPrefix prefix name =
    String.join "_" [ "", prefix, compress "_" name ]


compress : String -> String -> String
compress replaceBlank str =
    str
        |> String.toLower
        |> String.replace " " replaceBlank
        |> userReplace "[,;.!?&_]" (\_ -> "")


userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string


sectionPrefix : Int -> String
sectionPrefix level =
    case level of
        1 ->
            "section"

        2 ->
            "subsection"

        3 ->
            "subsubsection"

        _ ->
            "asection"



-- TITLE


maketitle : LaTeXState -> Html msg
maketitle latexState =
    let
        title =
            LaTeXState.getDictionaryItem "title" latexState

        author =
            LaTeXState.getDictionaryItem "author" latexState

        date =
            LaTeXState.getDictionaryItem "date" latexState

        email =
            LaTeXState.getDictionaryItem "email" latexState

        revision =
            LaTeXState.getDictionaryItem "revision" latexState

        revisionText =
            if revision /= "" then
                "Last revised " ++ revision

            else
                ""

        titlePart =
            Html.div [ HA.style "font-size" "28px", HA.style "padding-bottom" "12px" ] [ Html.text <| title ]

        authorPart =
            Html.div [ HA.style "font-size" "18px", HA.style "padding-bottom" "4px" ] [ Html.text <| author ]

        bodyParts =
            [ date, revisionText, email ]
                |> List.filter (\x -> x /= "")
                |> List.map (\x -> Html.div [ HA.style "font-size" "14px" ] [ Html.text x ])
    in
    Html.div []
        (titlePart :: authorPart :: bodyParts)



-- STUFF


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


rmad : String -> b -> LaTeXState -> List Expression -> SourceMap -> List (Attribute LaTeXMsg) -> Html LaTeXMsg
rmad si ms state args sm st =
    render si state args |> Html.div (st ++ active sm si)


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
    let
        args_ =
            Parser.renderToStingList args

        name =
            Parser.getStringAtWithDefault 0 "NAME" args_

        prefix =
            "section"

        compress_ str =
            str |> String.toLower |> String.replace " " ""

        ref =
            Debug.log "REF" <|
                String.join "_" [ "", prefix, compress_ name ]
    in
    labelFunction state :: render si state args |> Html.span (st ++ active sm si ++ [ HA.id ref ])


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



-- COLORED


colored : LaTeXState -> List Expression -> Html msg
colored latexState args =
    -- TODO
    let
        args_ =
            renderToStingList args

        lang : String -> Result (List DeadEnd) SyntaxHighlight.HCode
        lang =
            getLang (getStringAtWithDefault 0 "LANG" args_)

        theCode =
            getStringAtWithDefault 1 "CODE" args_
    in
    lang theCode
        |> Result.map SyntaxHighlight.toInlineHtml
        |> Result.withDefault
            (Html.code [] [ Html.text "isEmpty : String -> Bool" ])


getLang langString =
    case langString of
        "elm" ->
            SyntaxHighlight.elm

        "haskell" ->
            SyntaxHighlight.elm

        "js" ->
            SyntaxHighlight.javascript

        "xml" ->
            SyntaxHighlight.xml

        "css" ->
            SyntaxHighlight.css

        "python" ->
            SyntaxHighlight.python

        "sql" ->
            SyntaxHighlight.sql

        "json" ->
            SyntaxHighlight.json

        "nolang" ->
            SyntaxHighlight.noLang

        _ ->
            SyntaxHighlight.noLang


{-| rmas = render macro as code
-}
rmac si ms state args sm st =
    render si state args |> Html.code (st ++ active sm si)



-- ELLIE


ellie : List Expression -> Html msg
ellie args =
    let
        args_ =
            renderToStingList args

        id =
            getStringAtWithDefault 0 "0" args_

        url =
            "https://ellie-app.com/embed/" ++ id

        title_ =
            getStringAtWithDefault 1 "TITLE" args_

        title =
            if title_ == "xxx" then
                "Link to Ellie"

            else
                title_
    in
    Html.iframe [ HA.src url, HA.width 500, HA.height 600 ] [ Html.text title ]



-- IFRAME


iframe : List Expression -> Html msg
iframe args =
    let
        args_ =
            renderToStingList args

        url =
            getStringAtWithDefault 0 "URL" args_

        title =
            getStringAtWithDefault 1 "TITLE" args_
    in
    Html.iframe
        [ HA.src url
        , HA.width 400
        , HA.height 500
        , HA.attribute "Content-Type" "application/pdf"
        , HA.attribute "Content-Disposition" "inline"
        ]
        [ Html.text title ]
