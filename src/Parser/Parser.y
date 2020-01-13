{
module Parser.Parser (happyParser) where

import Lexer.Lexer (Token(..))
}

%name happyParser
%tokentype { Token }
%error { parseError }

%token
        int             { TokenInt $$}
        '+'             { TokenPlus }
        '-'             { TokenMinus }

%left '+' '-'

%%

Exp
	: Exp '+' Exp			{ Plus $1 $3 }
	| Exp '-' Exp			{ Minus $1 $3 }
	| int					{ Int $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
	= Plus Exp Exp
	| Minus Exp Exp
	| Int Int
	deriving Show

}
