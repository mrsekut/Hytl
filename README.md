# hytl

## Build

`$ stack build`

## Run

`$ stack exec hytl-exe`<br>

ex. `> 1+1`

## Create Lexer and Parser

- `$ alex lexer.x -o ./Lexer.hs`
- `$ happy parser.y -o ./Parser.hs`
