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
    }


type Msg
    = NoOp
    | InputText String
    | ReverseText


type alias Flags =
    {}
initialText : String
initialText = """This is a test: $a^2 + b^2 = c^2$"""

--renderFormatted : String -> String
--renderFormatted = render >> fo

render : String -> List String
render input =
   input |> Parser.Document.process
       |> Parser.Document.toParsed
       |> List.map Debug.toString
       |> String.join "\n\n"
       |> String.replace "," ",\n"
       |> formatted



options = { maximumWidth = 105
              , optimalWidth = 90
              , stringWidth = String.length
              }



formatted str = Paragraph.lines options str


{-
    process : String -> State

-}
init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = initialText
      , output = render initialText
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
            ( { model | input = str, output = formatted str }, Cmd.none )

        ReverseText ->
            ( { model | output = formatted  model.input }, Cmd.none )



--
-- VIEW
--

fontGray g = Font.color (Element.rgb g g g )
bgGray g =  Background.color (Element.rgb g g g)

view : Model -> Html Msg
view model =
    Element.layout [bgGray 0.2] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 36, width (px 600), height (px 700) ]
            [ title "MiniLaTeX B: Test"
            , inputText model
            , outputDisplay model
            , renderedTextDisplay model
            --, appButton
            ]
        ]

renderedTextDisplay model =
        column [ spacing 8 ]
            [ el [fontGray 0.9] (text "Rendered text")
            , renderedTextDisplay_ model]

renderedTextDisplay_ : Model -> Element msg
renderedTextDisplay_ model =
    column [ spacing 8
             , Font.size 14
             , Background.color (Element.rgb 1.0 1.0 1.0)
             , paddingXY 8 12
            , width (px 600)
            , height (px 150)
            ]
        (render2 model.input)

-- render2 : String -> List (Html msg)

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



outputDisplay : Model -> Element msg
outputDisplay model =
    column [ spacing 8 ]
        [ el [fontGray 0.9] (text "Output")
        , outputDisplay_ model]

outputDisplay_ : Model -> Element msg
outputDisplay_ model =
    column [ spacing 8
             , Font.size 14
             , Background.color (Element.rgb 1.0 1.0 1.0)
             , paddingXY 8 12
            , width (px 600)
            , height (px 150)]
        (render model.input |> List.map text)


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ width (px 600), height (px 200), Font.size 14]
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelAbove [fontGray 0.9] <| el [] (text "Input")
        , spellcheck = False
        }


appButton : Element Msg
appButton =
    row [  ]
        [ Input.button buttonStyle
            { onPress = Just ReverseText
            , label = el [ centerX, centerY ] (text "Reverse")
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
    [ Background.color (Element.rgb 0.5 0.5 1.0)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]



--