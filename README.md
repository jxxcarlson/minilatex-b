# MiniLaTeX

MiniLaTeX is a subset/variant of LaTeX that can
be turned into Html using the compiler provided by 
this package.  For simple applications, it is enough to 
do this, as illustrated in the app of `./simple-demo`:

```elm
MiniLaTeX.compile document
```


## Example

You can use `MiniLaTeX.compile` like this

```elm
import MiniLaTeX
import LaTeXMsg exposing(LaTeXMsg(..))

viewRenderedText : Element Msg
viewRenderedText =
    column [ .. ]
        (MiniLaTeX.compile Data.document
            |> List.map (Html.map LaTeXMsg)
            |> List.map Element.html
        )
        
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LaTeXMsg latexMsg ->
            ( model, Cmd.none )
        ..,


```

If some interactivity is needed, e.g., responding
to clicks in the rendered text, you must handle `latexMsg` in
the update function.


## Interactive Editing

The MiniLaTeX compiler offers a more sophisticated interface for
apps that feature interactive editing with real-time rendering
to Html.  See the documentation of the **MiniLaTeX** module for more
on this.

For routine uses, the **MiniLaTeX** module in this package 
should suffice.