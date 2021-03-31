module LaTeXMsg exposing (LaTeXMsg(..))

{-| LaTexMsg: the type of messages that can be sent by interacting with the rendered text.

@docs LaTeXMsg

-}

import Parser.Expression exposing (SourceMap)


{-| When the user clicks on an element in the rendered text,
this message is sent.  The corresponding element in the source
text can be highlighted using the LaTeXMsg.
-}
type LaTeXMsg
    = SendSourceMap SourceMap
