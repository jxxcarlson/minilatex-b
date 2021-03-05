# Notes

## Milestones

- Robust parser passing tests with good error recovery

- Implement the existing rendereing functions of MiniLaTeX A.

- Render with LaTeXState accumulator

- Differential parse and render

- Sync: Rendered to Source

- Sync: Source to Renderered

- LaTeX export

- String export

- Integration with simple demo.

- Integration with editor

## To do

- Nested environments are not properly parsed.  This is 
  likely  a problem with the code in `Parser.Document` 
  that splits the input into logical blocks of text.