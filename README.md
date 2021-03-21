# MiniLaTeX

MiniLaTeX is a subset/variant of LaTeX that can
be turned into Html using the compiler provided by 
this package.  For simple applications, it is enough to 
use.

    MiniLaTeX.compile document

This function takes a list of strings representing your MiniLaTeX
document as input and produces Html for your web app.
For more details, see the app `./simple-demo`.\`

## Interactive editing

For applications that use interactive editing, use _LaTeXData_
and the functions _init_, _update_ and _viewLaTeXData_. If
your app is written with mdgriffith/elm-ui, use
_viewLaTeXDataAsElement_ instead.

The function init will set up a LaTeXData value.
It carries all the information needed for
efficient interactive editing. Successive
changes to it as edits are made to the document
are made using _update_.


## Example

You can use `MiniLaTeX.compile` like this

```elm
import MiniLaTeX
import LaTeXMsg exposing(LaTeXMsg(..))

viewRenderedText : Element Msg
viewRenderedText =
    column [ .. ]
        (MiniLaTeX.compile Data.document
            |> List.map (Html.map LaTeXMsg >> Element.html)
        )
        
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LaTeXMsg latexMsg ->
            ( model, Cmd.none )
        ..,


```
module for more on this.  Also the code in `./simple-demo`.

## Notes

For more information on how this package operates,
see the included `./src/Architecture.md`

