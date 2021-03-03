module Config exposing (..)

import Html
import Html.Attributes as HA


lineHeight =
    HA.style "line-height" "1.5"


inlineMathStyle =
    []


displayMathStyle =
    []


redColor =
    "#a00"


blueColor =
    "#00c"


textSpanStyle =
    [ lineHeight ]


errorStyle =
    [ HA.style "color" "#0000FF", HA.style "background-color" "pink", HA.style "padding" "4px" ]


errorStyle2 =
    [ HA.style "color" "#0000FF", HA.style "background-color" "#EE82EE", HA.style "padding" "4px" ]
