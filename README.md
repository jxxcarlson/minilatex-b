# MiniLaTeX

MiniLaTeX is a subset/variant of LaTeX that can
be turned into Html using the compiler provided by 
this package.  For simple applications, it is enough to 
do this, as illustrated in the app of `./simple-demo`:

```elm
MiniLaTeX.compile document
```

The MiniLaTeX compiler offers a more sophisticated interface for
apps that feature interactive editing with real-time rendering
to Html.  See the documentation of the **MiniLaTeX** module for more
on this.

For routine uses, the **MiniLaTeX** module in this package 
should suffice.