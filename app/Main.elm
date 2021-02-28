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
import List.Extra
import Paragraph
import Parser.Document
import Parser.Expression exposing (Expression)
import Parser.Parser as PP exposing (..)
import Render.Render as Render exposing (LaTeXMsg)


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


type RHViewMode
    = RHRenderedText
    | RHAnnotatedSource


type FooterViewMode
    = ShowParsedText
    | ShowParseErrors
    | ShowSourceMap


type Msg
    = NoOp
    | InputText String
    | CycleViewMode
    | CycleRHViewMode
    | LaTeXMsg LaTeXMsg
    | InputCommand String
    | RunCommand


type alias Flags =
    {}


initialText : String
initialText =
    """
This is a test: $a^2 + b^2 = c^2

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
      , sourceMapIndex = Parser.Expression.sourceMapIndex (parse initialText)
      , counter = 0
      , lhViewMode = LHSourceText
      , rhViewMode = RHRenderedText
      , footerViewMode = ShowParsedText
      , message = ""
      , command = ""
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

        CycleRHViewMode ->
            let
                rhViewMode =
                    case model.rhViewMode of
                        RHAnnotatedSource ->
                            RHRenderedText

                        RHRenderedText ->
                            RHAnnotatedSource
            in
            ( { model | rhViewMode = rhViewMode }, Cmd.none )

        CycleViewMode ->
            let
                viewMode =
                    case model.footerViewMode of
                        ShowParsedText ->
                            ShowParseErrors

                        ShowParseErrors ->
                            ShowSourceMap

                        ShowSourceMap ->
                            ShowParsedText
            in
            ( { model | footerViewMode = viewMode }, Cmd.none )

        InputCommand command ->
            ( { model | command = command }, Cmd.none )

        RunCommand ->
            case CommandInterpreter.get model.command of
                Just command ->
                    case command.name of
                        "locate" ->
                            let
                                sm =
                                    { chunkOffset = CommandInterpreter.getIntArg 0 command.args |> Debug.log "CO"
                                    , length = 0
                                    , offset = 0
                                    }
                            in
                            ( { model | message = Render.locate sm model.input |> Debug.toString }, Cmd.none )

                        _ ->
                            ( { model | message = Debug.toString command }, Cmd.none )

                _ ->
                    ( { model | message = "No command" }, Cmd.none )

        LaTeXMsg sourceMap ->
            ( { model | message = Debug.toString sourceMap }, Cmd.none )



--( { model | message = Debug.toString (Render.locate sourceMap model.input) }, Cmd.none )
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

            --, row [ spacing 12 ] [ inputText model, annotatedText model ]
            --, row [ spacing 12 ] [ renderedTextDisplay model, messageDisplay model ]
            , parsedTextDisplay model
            ]
        ]


lhView : Model -> Element Msg
lhView model =
    case model.lhViewMode of
        LHSourceText ->
            inputText model


rhView : Model -> Element Msg
rhView model =
    case model.rhViewMode of
        RHRenderedText ->
            renderedTextDisplay model

        RHAnnotatedSource ->
            annotatedText model


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


annotatedText model =
    column
        [ spacing 8
        , moveUp 8
        ]
        [ rhViewModeButton model
        , annotatedText_ model
        ]


annotatedText_ model =
    column
        [ spacing 12
        , Font.size 14
        , Background.color (Element.rgb 0.9 0.9 1.0)
        , paddingXY 8 12
        , width (px panelWidth)
        , height (px panelHeight)
        ]
        -- (List.map (\s -> el [] (Element.text s)) (String.lines model.input))
        (Render.highLight model.input model.parsedText |> List.map Element.html)


renderedTextDisplay model =
    column [ spacing 8, moveUp 8 ]
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
        |> Parser.Document.toParsed
        |> List.map (Render.render >> Html.div [ HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5" ])
        |> Html.div []
    )
        |> Html.map LaTeXMsg


render2 : String -> List (Element Msg)
render2 input =
    input
        |> Parser.Document.process
        |> Parser.Document.toParsed
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
            model.parsedText |> PP.getErrors |> parsedTextToString_ |> renderParsedText

        ShowSourceMap ->
            model.parsedText
                |> List.map Parser.Expression.getSourceOfList
                |> List.map Parser.Expression.sourceMapToString
                |> renderParsedText



--|> List.map .chunkOffset
--|> Parser.Expression.makeIndex
--|> List.map (Debug.toString >> Element.text)


renderParsedText : List String -> List (Element Msg)
renderParsedText =
    List.indexedMap (\k s -> row [ spacing 8 ] [ Element.text (String.fromInt k), Element.text s ])


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ width (px panelWidth), height (px panelHeight), Font.size 16 ]
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelAbove [ fontGray 0.9 ] <| el [ Font.size 16 ] (Element.text "Source Text")
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

                RHAnnotatedSource ->
                    "Annotated Source"
    in
    row []
        [ Input.button buttonStyle
            { onPress = Just CycleRHViewMode
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
