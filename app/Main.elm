module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import CommandInterpreter exposing (Command)
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
import Parser.Parser as Parser
import Render.Render as Render exposing (LaTeXMsg(..))


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
    , sourceMap : Parser.Expression.SourceMap
    , sourceMapIndex : List (List Int)
    , counter : Int
    , footerViewMode : FooterViewMode
    , lhViewMode : LHViewMode
    , rhViewMode : RHViewMode
    , message : String
    , command : String
    }


type LHViewMode
    = LHSourceText
    | LHAnnotatedSource


type RHViewMode
    = RHRenderedText


type FooterViewMode
    = ShowParsedText
    | ShowParseErrors
    | ShowSourceMap
    | ShowSourceMapIndex


type Msg
    = NoOp
    | InputText String
    | CycleViewMode
    | CycleRHViewMode
    | CycleLHViewMode
    | LaTeXMsg LaTeXMsg
    | InputCommand String
    | RunCommand


type alias Flags =
    {}


initialText : String
initialText =
    """This is a test: $a^2 + b^2 = c^2

\\strong{\\italic{More stuff:}} $p^2 \\equiv 1$
one
two
three

$$

Still more stuff
"""


parse : String -> List (List Expression)
parse input =
    input
        |> Parser.Document.process


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
      , sourceMap = Parser.Expression.dummySourceMap
      , sourceMapIndex = Parser.Expression.sourceMapIndex (numberOfLines initialText) (parse initialText)
      , counter = 0
      , lhViewMode = LHSourceText
      , rhViewMode = RHRenderedText
      , footerViewMode = ShowParsedText
      , message = ""
      , command = ""
      }
    , Cmd.none
    )


numberOfLines : String -> Int
numberOfLines str =
    str |> String.lines |> List.length


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            let
                parsedText =
                    parse str
            in
            ( { model
                | input = str
                , parsedText = parsedText
                , sourceMapIndex = Parser.Expression.sourceMapIndex (numberOfLines str) parsedText
                , counter = model.counter + 1
              }
            , Cmd.none
            )

        CycleLHViewMode ->
            let
                lhViewMode =
                    case model.lhViewMode of
                        LHSourceText ->
                            LHAnnotatedSource

                        LHAnnotatedSource ->
                            LHSourceText
            in
            ( { model | lhViewMode = lhViewMode }, Cmd.none )

        CycleRHViewMode ->
            ( model, Cmd.none )

        CycleViewMode ->
            let
                viewMode =
                    case model.footerViewMode of
                        ShowParsedText ->
                            ShowParseErrors

                        ShowParseErrors ->
                            ShowSourceMap

                        ShowSourceMap ->
                            ShowSourceMapIndex

                        ShowSourceMapIndex ->
                            ShowParsedText
            in
            ( { model | footerViewMode = viewMode }, Cmd.none )

        InputCommand command ->
            ( { model | command = command }, Cmd.none )

        RunCommand ->
            case CommandInterpreter.get model.command of
                Just command ->
                    case command.name of
                        "at" ->
                            let
                                k =
                                    CommandInterpreter.getIntArg 0 command.args
                            in
                            ( { model | message = Render.at k model.input |> Debug.toString }, Cmd.none )

                        _ ->
                            ( { model | message = Debug.toString command }, Cmd.none )

                _ ->
                    ( { model | message = "No command" }, Cmd.none )

        LaTeXMsg (SendSourceMap sourceMap) ->
            ( { model
                | sourceMap = sourceMap
                , lhViewMode = LHAnnotatedSource
                , message = Parser.Expression.getSelectionFromSourceMap sourceMap model.input model.sourceMapIndex
              }
            , Cmd.none
            )



--
-- VIEW
--


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ focusStyle noFocus ] } [ bgGray 0.2, clipX, clipY ] (mainColumn model)


panelHeight : Int
panelHeight =
    380


appWidth : Int
appWidth =
    900


panelWidth =
    appWidth // 2


parsedTextPeneWith =
    1200


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column
            [ spacing 36
            , width (px (appWidth + 40))
            , height (px 700)
            , paddingXY 20 0
            , clipX
            , clipY
            ]
            [ row [ spacing 8 ] [ inputCommand model, runCommandButton ]
            , row [ spacing 12 ] [ lhView model, rhView model ]
            , parsedTextDisplay model
            ]
        ]


lhView : Model -> Element Msg
lhView model =
    case model.lhViewMode of
        LHSourceText ->
            inputText model

        LHAnnotatedSource ->
            annotatedText model


rhView : Model -> Element Msg
rhView model =
    case model.rhViewMode of
        RHRenderedText ->
            renderedTextDisplay model


messageDisplay model =
    column [ spacing 8 ]
        [ el
            [ fontGray 0.9
            , Font.size 14
            , height (px 35)
            , width (px 300)
            , bgGray 0.9

            --  , Font.color (bgGray 0.1)
            ]
            (Element.text "Messages")
        , messageDisplay_ model
        ]


messageDisplay_ model =
    el [ Font.size 14, fontGray 0.9 ] (Element.text model.message)


annotatedText : Model -> Element Msg
annotatedText model =
    column
        [ spacing 8
        , moveUp 0
        ]
        [ lhViewModeButton model
        , annotatedText_ model
        ]


annotatedText_ : Model -> Element msg
annotatedText_ model =
    column
        [ spacing 12
        , Font.size 14
        , Background.color (Element.rgb 0.9 0.9 1.0)
        , paddingXY 8 12
        , width (px panelWidth)
        , height (px panelHeight)
        ]
        [ Render.highlightWithSourceMap model.sourceMap model.input model.sourceMapIndex |> Element.html ]


renderedTextDisplay model =
    column [ spacing 8, moveUp 0 ]
        [ row [ spacing 12 ] [ rhViewModeButton model ]
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
        , scrollbarY
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
    (input
        |> Parser.Document.process
        |> List.map (Render.render >> Html.div [ HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5" ])
        |> Html.div []
    )
        |> Html.map LaTeXMsg


render2 : String -> List (Element Msg)
render2 input =
    input
        |> Parser.Document.process
        |> List.map (Render.render >> Html.div [ HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5" ])
        |> List.map (Element.html >> Element.map LaTeXMsg)


title : String -> Element Msg
title str =
    row [ centerX, Font.bold, fontGray 0.9 ] [ Element.text str ]


parsedTextDisplay : Model -> Element Msg
parsedTextDisplay model =
    column [ spacing 8 ]
        [ row [ spacing 12, width (px 400) ] [ footerViewModeButton model, messageDisplay_ model ]
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
        , height (px 150)
        , Element.htmlAttribute (HA.style "line-height" "1.5")
        , scrollbarY
        , scrollbarX
        ]
        (renderParseResult model)


renderParseResult : Model -> List (Element Msg)
renderParseResult model =
    case model.footerViewMode of
        ShowParsedText ->
            model.parsedText |> parsedTextToString |> renderParsedText

        ShowParseErrors ->
            model.parsedText |> Parser.getErrors |> parsedTextToString_ |> renderParsedText

        ShowSourceMap ->
            model.parsedText
                |> List.map Parser.Expression.getSourceOfList
                |> List.map Parser.Expression.sourceMapToString
                |> renderParsedText

        ShowSourceMapIndex ->
            model.sourceMapIndex
                |> List.map (Debug.toString >> (\x -> el [] (Element.text x)))


renderParsedText : List String -> List (Element Msg)
renderParsedText =
    List.indexedMap (\k s -> row [ spacing 8 ] [ Element.text (String.fromInt k), Element.text s ])


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ width (px panelWidth), height (px panelHeight), Font.size 16 ]
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelAbove [] <| lhViewModeButton model
        , spellcheck = False
        }


inputCommand : Model -> Element Msg
inputCommand model =
    Input.text [ width (px panelWidth), height (px 35), centerY, Font.size 14 ]
        { onChange = InputCommand
        , text = model.command
        , placeholder = Nothing
        , label = Input.labelHidden "inputCommand"
        }



-- BUTTONS


rhViewModeButton : Model -> Element Msg
rhViewModeButton model =
    let
        title_ =
            case model.rhViewMode of
                RHRenderedText ->
                    "Rendered Text"
    in
    row []
        [ Input.button buttonStyleInactive
            { onPress = Just CycleRHViewMode
            , label = el [ centerX, centerY ] (Element.text title_)
            }
        ]


lhViewModeButton : Model -> Element Msg
lhViewModeButton model =
    let
        title_ =
            case model.lhViewMode of
                LHAnnotatedSource ->
                    "Annotated text"

                LHSourceText ->
                    "Source text"
    in
    row []
        [ Input.button buttonStyle
            { onPress = Just CycleLHViewMode
            , label = el [ centerX, centerY ] (Element.text title_)
            }
        ]


runCommandButton =
    Input.button buttonStyle
        { onPress = Just RunCommand
        , label = el [] (Element.text "Run command")
        }


footerViewModeButton : Model -> Element Msg
footerViewModeButton model =
    let
        title_ =
            case model.footerViewMode of
                ShowParseErrors ->
                    "Parse Errors"

                ShowParsedText ->
                    "Parsed Text"

                ShowSourceMap ->
                    "SourceMap"

                ShowSourceMapIndex ->
                    "SourceMap Index"
    in
    row []
        [ Input.button buttonStyle
            { onPress = Just CycleViewMode
            , label = el [ centerX, centerY ] (Element.text title_)
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
    [ Background.color (Element.rgb 0.25 0.25 0.8)
    , Font.color (rgb255 255 255 255)
    , Font.size 14
    , paddingXY 15 8
    ]


buttonStyleInactive =
    [ Background.color (Element.rgb 0.25 0.25 0.25)
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
