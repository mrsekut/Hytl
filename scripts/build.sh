#!/bin/bash

# alex
alex src/Lexer/lexer.x -o src/Lexer/Lexer.hs

# happy
happy src/Parser/parser.y -o src/Parser/Parser.hs

# build
stack build
