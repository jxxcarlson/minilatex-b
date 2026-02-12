module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Data.MiniLaTeXIO
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
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
    { sourceText: String
    , input : String
    , output : String
    , viewMode : ViewMode
    , fontSize : Int
    }


windowHeight : Int
windowHeight =
    550


type Msg
    = LaTeXMsg LaTeXMsg
    | SetViewMode ViewMode
    | ChangeFontSize Direction


type Direction
    = Up
    | Down


type ViewMode
    = Paper
    | Light
    | Dark


bgColor : ViewMode -> Attr decorative msg
bgColor viewMode =
    case viewMode of
        Paper ->
            Background.color (Element.rgb255 255 255 255)

        Light ->
            Background.color (Element.rgb255 255 255 245)

        Dark ->
            Background.color (Element.rgb255 71 70 70)


fgColor : ViewMode -> Attr decorative msg
fgColor viewMode =
    case viewMode of
        Paper ->
            Font.color (Element.rgb255 40 40 40)

        Light ->
            Font.color (Element.rgb255 40 40 40)

        Dark ->
            Font.color (Element.rgb255 240 240 240)


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = "App started"
      , output = "App started"
      , sourceText = Data.MiniLaTeXIO.sourceText
      , viewMode = Light
      , fontSize = 14
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

        SetViewMode viewMode ->
            ( { model | viewMode = viewMode }, Cmd.none )

        ChangeFontSize direction ->
            case direction of
                Up ->
                    ( { model | fontSize = model.fontSize + 1 }, Cmd.none )

                Down ->
                    ( { model | fontSize = model.fontSize - 1 }, Cmd.none )



--
-- VIEW
--


bgGray g =
    Background.color (Element.rgb g g g)


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ focusStyle noFocus ] } [ bgGray 0.2, width fill, height fill ] (mainColumn model)


noFocus : Element.FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 18, width (px 1010), height fill ]
            [ titleBar "MiniLaTeX: Simple Demo (work in progress, new compiler)"
            , row [ spacing 10 ] [ viewSourceText model, viewRenderedText model ]
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
        , spacing 24
        ]
        [ paperModeButtom model.viewMode
        , lightModeButtom model.viewMode
        , darkModeButtom model.viewMode
        , row [ spacing 8 ] [ el [] (text "Font Size"), el [] (text <| String.fromInt model.fontSize), increaseFontSizeButton, decreaseFontSizeButton ]
        ]


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


viewSourceText : Model -> Element Msg
viewSourceText model =
    column [ width (px 500), height (px windowHeight), scrollbarY, Font.size model.fontSize, Background.color (Element.rgb255 240 240 240) ]
        [ el [ paddingXY 20 20 ] (Element.text model.sourceText) ]


viewRenderedText : Model -> Element Msg
viewRenderedText model =
    column
        [ width (px 500)
        , height (px windowHeight)
        , scrollbarY
        , Font.size model.fontSize
        , fgColor model.viewMode
        , bgColor model.viewMode
        , padding 20
        ]
        (MiniLaTeX.compileFromString "demo" model.sourceText
            |> List.map (\h -> el [ width fill ] (Element.html (Html.map LaTeXMsg h)))
        )



-- BUTTONS


mouseDown =
    Element.mouseDown [ Background.color (Element.rgb 140 0 0), Font.color (Element.rgb255 20 20 20) ]


buttonStyle flag =
    if flag then
        [ mouseDown
        , padding 6
        , Font.color (Element.rgb255 220 220 255)
        , Font.size 14
        , Background.color (Element.rgb255 20 20 160)
        ]

    else
        [ mouseDown
        , padding 6
        , Font.color (Element.rgb255 220 220 255)
        , Font.size 14
        , Background.color (Element.rgb255 60 60 60)
        ]


paperModeButtom viewMode =
    Input.button (buttonStyle (viewMode == Paper))
        { onPress = Just (SetViewMode Paper)
        , label = el [] (text "Paper mode")
        }


lightModeButtom viewMode =
    Input.button (buttonStyle (viewMode == Light))
        { onPress = Just (SetViewMode Light)
        , label = el [] (text "Light mode")
        }


increaseFontSizeButton =
    Input.button (buttonStyle False)
        { onPress = Just (ChangeFontSize Up)
        , label = el [] (text (String.fromChar '↑'))
        }


decreaseFontSizeButton =
    Input.button (buttonStyle False)
        { onPress = Just (ChangeFontSize Down)
        , label = el [] (text (String.fromChar '↓'))
        }


darkModeButtom viewMode =
    Input.button (buttonStyle (viewMode == Dark))
        { onPress = Just (SetViewMode Dark)
        , label = el [] (text "Dark mode")
        }



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


barBG =
    Element.rgb255 100 100 110


barFontSize =
    Font.size 16


barFontColor =
    Element.rgb255 240 240 250
