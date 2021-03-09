module LaTeXMsg exposing (LaTeXMsg(..))

{-| LaTexMsg: the type of messages that can be sent by interacting with the rendered text.

@docs LaTeXMsg

-}

import Parser.Expression exposing (SourceMap)


{-| When the user clicks on an element in the rendered text,
this message is sent.
-}
type LaTeXMsg
    = SendSourceMap SourceMap
