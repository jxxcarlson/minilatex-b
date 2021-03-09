module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Data
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import LaTeXMsg exposing (LaTeXMsg(..))
import MiniLaTeX


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , output : String
    }


windowHeight : Int
windowHeight =
    550


type Msg
    = LaTeXMsg LaTeXMsg


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = "App started"
      , output = "App started"
      }
    , Cmd.none
    )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LaTeXMsg _ ->
            ( model, Cmd.none )



--
-- VIEW
--


bgGray g =
    Background.color (Element.rgb g g g)


view : Model -> Html Msg
view model =
    Element.layout [ bgGray 0.2, width fill, height fill ] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 18, width (px 1010), height fill ]
            [ titleBar "MiniLaTeX: Simple Demo (work in progress, new compiler)"
            , row [ spacing 10 ] [ viewSourceText, viewRenderedText ]
            , footer model
            ]
        ]


footer : Model -> Element Msg
footer model =
    row
        [ centerX
        , width (px 1010)
        , paddingXY 15 5
        , height (px 40)
        , Font.color barFontColor
        , barFontSize
        , Background.color barBG
        ]
        [ text "FOOTER" ]


barBG =
    Element.rgb255 100 100 110


barFontSize =
    Font.size 16


barFontColor =
    Element.rgb255 240 240 250


titleBar : String -> Element msg
titleBar str =
    row
        [ centerX
        , width (px 1010)
        , paddingXY 15 5
        , height (px 40)
        , barFontSize
        , Font.color barFontColor
        , Background.color barBG
        ]
        [ text str ]


viewSourceText : Element Msg
viewSourceText =
    column [ width (px 500), height (px windowHeight), scrollbarY, Font.size 14, Background.color (Element.rgb255 240 240 240) ]
        [ el [ paddingXY 20 20 ] (Element.text Data.document) ]


viewRenderedText : Element Msg
viewRenderedText =
    column [ width (px 500), height (px windowHeight), scrollbarY, Font.size 14 ]
        (MiniLaTeX.compile Data.document
            |> List.map (Html.map LaTeXMsg)
            |> List.map Element.html
        )



--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , Background.color (Element.rgb255 255 255 245)

    -- , Background.color (Element.rgb255 71 70 70)
    --, Font.color (Element.rgb255 205 200 200)
    , paddingXY 20 20
    ]
