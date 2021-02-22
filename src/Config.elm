module Config exposing (..)

import Html
import Html.Attributes as HA

lineHeight =  HA.style "line-height" "1.5"

inlineMathStyle = []

displayMathStyle = []

textSpanStyle = [lineHeight]

errorStyle = [HA.style "color" "#900", HA.style "width" "500px", HA.style "padding" "15px"]