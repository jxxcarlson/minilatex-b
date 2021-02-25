module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Element exposing (..)
import Html.Keyed
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Parser.Parser as PP exposing(..)
import Parser.Document
import Paragraph
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
    , output : List String
    , counter : Int
    }


type Msg
    = NoOp
    | InputText String


type alias Flags =
    {}
initialText : String
initialText = """This is a test: $a^2 + b^2 = c^2$"""

--renderFormatted : String -> String
--renderFormatted = render >> fo

--parse : String -> List String

parse : String -> List String
parse input =
   input |> Parser.Document.process
       |> Parser.Document.toParsed
       |> List.map Debug.toString
       |> List.map (formatted >> String.join "\n")


options = { maximumWidth = 200
              , optimalWidth = 190
              , stringWidth = String.length
              }


formatted : String -> List String
formatted str = Paragraph.lines options str


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = initialText
      , output = parse initialText
      , counter = 0
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
            ( { model | input = str, output = formatted str, counter = model.counter + 1}, Cmd.none)



--
-- VIEW
--

fontGray g = Font.color (Element.rgb g g g )
bgGray g =  Background.color (Element.rgb g g g)

view : Model -> Html Msg
view model =
    Element.layout [bgGray 0.2] (mainColumn model)

panelHeight : Int
panelHeight = 150

panelWidth : Int
panelWidth = 1200

mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 36, width (px (panelWidth + 40)), height (px 700), paddingXY 20 0 ]
            [ title "MiniLaTeX B: Test"
            ,  inputText model
            , renderedTextDisplay model
            , parsedTextDisplay model

            --, appButton
            ]
        ]

renderedTextDisplay model =
        column [ spacing 8 ]
            [ el [fontGray 0.9, Font.size 16] (text "Rendered text")
            , renderedTextDisplay_ model]

renderedTextDisplay_ : Model -> Element Msg
renderedTextDisplay_ model =
    column [ spacing 8
             , Font.size 14
             , Background.color (Element.rgb 1.0 1.0 1.0)
             , Background.color (Element.rgb 1.0 1.0 1.0)
             , paddingXY 8 12
            , width (px 600)
            , height (px panelHeight)
            ]
        [ mathNode model.counter model.input]


mathNode: Int -> String -> Element Msg
mathNode counter content =
    Html.Keyed.node "div" [] [(String.fromInt counter, render1 content)]
      |> Element.html



render1 : String -> Html Msg
render1 input =
   input
     |> Parser.Document.process
     |> Parser.Document.toParsed
     |> List.map (Render.render >> Html.div [HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5"])
     |> Html.div []

render2 : String -> List (Element nsg)
render2 input =
   input
     |> Parser.Document.process
     |> Parser.Document.toParsed
     |> List.map (Render.render >> Html.div [HA.style "margin-bottom" "10px", HA.style "white-space" "normal", HA.style "line-height" "1.5"])
     |> List.map Element.html

title : String -> Element msg
title str =
    row [ centerX, Font.bold, fontGray 0.9 ] [ text str ]



parsedTextDisplay : Model -> Element msg
parsedTextDisplay model =
    column [ spacing 8 ]
        [ el [fontGray 0.9, Font.size 16] (text "Parsed text")
        , parsedTextDisplay_ model]

parsedTextDisplay_ : Model -> Element msg
parsedTextDisplay_ model =
    column [ spacing 16
             , Font.size 14
             , Background.color (Element.rgb 1.0 1.0 1.0)
             , paddingXY 8 12
            , width (px panelWidth)
            , height (px (panelHeight + 40) )]
        (parse model.input |> List.indexedMap (\k s -> row [spacing 8] [text (String.fromInt k), text s]))


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ width (px 600), height (px panelHeight), Font.size 16]
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelAbove [fontGray 0.9] <| el [Font.size 16] (text "Source Text")
        , spellcheck = False
        }

--
--appButton : Element Msg
--appButton =
--    row [  ]
--        [ Input.button buttonStyle
--            { onPress = Just ReverseText
--            , label = el [ centerX, centerY ] (text "Reverse")
--            }
--        ]



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
    [ Background.color (Element.rgb 0.5 0.5 1.0)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]



--