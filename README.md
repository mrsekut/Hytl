# Hytl

- Hytl lang is mrsekut's original toy programming language.
- [Docs](https://scrapbox.io/mrsekut-p/Hytl)

## Build

`$ ./build.sh`

## Run

`$ stack exec -- hytl-exe`

- ex. `> 1+1`
- out. `2`

### AST Mode

`$ stack exec -- hytl-exe -a`

- ex. `> 1+1`
- out. `Plus (Int 2) (Int 3)`

### Quit

`> :quit`

### Help

`$ stack exec -- hytl-exe -h`
