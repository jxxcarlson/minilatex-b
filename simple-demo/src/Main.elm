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
import MiniLaTeX
import Render.LaTeXState as LaTeXState


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


type Msg
    = LaTeXMsg LaTeXState.LaTeXMsg


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


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)


view : Model -> Html Msg
view model =
    Element.layout [ bgGray 0.2, width fill, height fill ] mainColumn


mainColumn : Element Msg
mainColumn =
    column mainColumnStyle
        [ column [ spacing 36, width (px 500), height fill ]
            [ title "MiniLaTeX: Simple Demo"
            , display
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX ] [ text str ]


display : Element Msg
display =
    column [ width (px 500), height (px 600), scrollbarY, Font.size 14 ]
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
