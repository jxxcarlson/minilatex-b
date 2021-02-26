module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Paragraph
import Parser.Document
import Parser.Expression exposing (Expression)
import Parser.Parser as PP exposing (..)
import Render


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , parsedText : List (List Expression)
    , counter : Int
    , viewMode : ViewMode
    }


type ViewMode
    = ShowParsedText
    | ShowParseErrors
    | ShowSourceMap


type Msg
    = NoOp
    | InputText String
    | CycleViewMode


type alias Flags =
    {}


initialText : String
initialText =
    """This is a test: $a^2 + b^2 = c^2$"""


parse : String -> List (List Expression)
parse input =
    input
        |> Parser.Document.process
        |> Parser.Document.toParsed


parsedTextToString : List (List Expression) -> List String
parsedTextToString pt =
    pt
        |> List.map Debug.toString
        |> List.map (formatted >> String.join "\n")


parsedTextToString_ : List Expression -> List String
parsedTextToString_ pt =
    pt
        |> List.map Debug.toString



-- (formatted >> String.join "\n")


options =
    { maximumWidth = 200
    , optimalWidth = 190
    , stringWidth = String.length
    }


formatted : String -> List String
formatted str =
    Paragraph.lines options str


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = initialText
      , parsedText = parse initialText
      , counter = 0
      , viewMode = ShowParsedText
      }
    , Cmd.none
    )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model | input = str, parsedText = parse str, counter = model.counter + 1 }, Cmd.none )

        CycleViewMode ->
            let
                viewMode =
                    case model.viewMode of
                        ShowParsedText ->
                            ShowParseErrors

                        ShowParseErrors ->
                            ShowSourceMap

                        ShowSourceMap ->
                            ShowParsedText
            in
            ( { model | viewMode = viewMode }, Cmd.none )



--
-- VIEW
--


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ focusStyle noFocus ] } [ bgGray 0.2 ] (mainColumn model)


panelHeight : Int
panelHeight =
    150


appWidth : Int
appWidth =
    1200


panelWidth =
    appWidth // 2


parsedTextPeneWith =
    1200


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 36, width (px (appWidth + 40)), height (px 700), paddingXY 20 0 ]
            [ title "MiniLaTeX B: Test"
            , row [ spacing 12 ] [ inputText model, annotatedText model ]
            , renderedTextDisplay model
            , parsedTextDisplay model
            ]
        ]


annotatedText model =
    column
        [ spacing 8
        ]
        [ el [ fontGray 0.9, Font.size 16 ] (text "Annotated source text")
        , annotatedText_ model
        ]


annotatedText_ model =
    column
        [ spacing 12
        , Font.size 14
        , Background.color (Element.rgb 0.9 0.9 1.0)
        , paddingXY 8 12
        , width (px panelWidth)
        , height (px (panelHeight - 4))
        ]
        (List.map (\s -> el [] (Element.text s)) (String.lines model.input))


renderedTextDisplay model =
    column [ spacing 8 ]
        [ el [ fontGray 0.9, Font.size 16 ] (text "Rendered text")
        , renderedTextDisplay_ model
        ]


renderedTextDisplay_ : Model -> Element Msg
renderedTextDisplay_ model =
    column
        [ spacing 8
        , Font.size 14
        , Background.color (Element.rgb 1.0 1.0 1.0)
        , Background.color (Element.rgb 1.0 1.0 1.0)
        , paddingXY 8 12
        , width (px panelWidth)
        , height (px panelHeight)
        ]
        [ mathNode model.counter model.input ]


mathNode : Int -> String -> Element Msg
mathNode counter content =
    Html.Keyed.node "div" [] [ ( String.fromInt counter, render1 content ) ]
        |> Element.html


render1 : String -> Html Msg
render1 input =
    input
        |> Parser.Document.process
        |> Parser.Document.toParsed
        |> List.map (Render.render >> Html.div [ HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5" ])
        |> Html.div []


render2 : String -> List (Element nsg)
render2 input =
    input
        |> Parser.Document.process
        |> Parser.Document.toParsed
        |> List.map (Render.render >> Html.div [ HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5" ])
        |> List.map Element.html


title : String -> Element msg
title str =
    row [ centerX, Font.bold, fontGray 0.9 ] [ text str ]


parsedTextDisplay : Model -> Element Msg
parsedTextDisplay model =
    column [ spacing 8 ]
        [ appButton model
        , parsedTextDisplay_ model
        ]


parsedTextDisplay_ : Model -> Element Msg
parsedTextDisplay_ model =
    column
        [ spacing 16
        , Font.size 14
        , Background.color (Element.rgb 1.0 1.0 1.0)
        , paddingXY 8 12
        , width (px (2 * panelWidth + 10))
        , height (px (panelHeight + 40))
        , Element.htmlAttribute (HA.style "line-height" "1.5")
        ]
        (renderParseResult model)


renderParseResult model =
    case model.viewMode of
        ShowParsedText ->
            model.parsedText |> parsedTextToString |> renderParsedText

        ShowParseErrors ->
            model.parsedText |> PP.getErrors |> parsedTextToString_ |> renderParsedText

        ShowSourceMap ->
            model.parsedText
                |> PP.getErrors
                |> List.map Parser.Expression.getSource
                |> List.map Parser.Expression.sourceMapToString
                |> renderParsedText


renderParsedText : List String -> List (Element Msg)
renderParsedText =
    List.indexedMap (\k s -> row [ spacing 8 ] [ text (String.fromInt k), text s ])


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ width (px panelWidth), height (px panelHeight), Font.size 16 ]
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelAbove [ fontGray 0.9 ] <| el [ Font.size 16 ] (text "Source Text")
        , spellcheck = False
        }


appButton : Model -> Element Msg
appButton model =
    let
        title_ =
            case model.viewMode of
                ShowParseErrors ->
                    "Parse Errors"

                ShowParsedText ->
                    "Parsed Text"

                ShowSourceMap ->
                    "SourceMaP"
    in
    row []
        [ Input.button buttonStyle
            { onPress = Just CycleViewMode
            , label = el [ centerX, centerY ] (text title_)
            }
        ]



--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , bgGray 0.5
    , paddingXY 20 20
    ]


buttonStyle =
    [ Background.color (Element.rgb 0.35 0.35 1.0)
    , Font.color (rgb255 255 255 255)
    , Font.size 14
    , paddingXY 15 8
    ]


noFocus : Element.FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }



--
