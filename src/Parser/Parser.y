{
module Parser.Parser (parse, Exp(..)) where

import Lexer.Lexer (Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }

%token
	int             { TokenInt $$}
	'+'             { TokenPlus }
	'-'             { TokenMinus }
	'*'             { TokenTimes }
	'/'             { TokenDiv }

%left '+' '-' '*' '/'

%%

Exp
	: Exp '+' Exp			{ Plus $1 $3 }
	| Exp '-' Exp			{ Minus $1 $3 }
	| Exp '*' Exp			{ Times $1 $3 }
	| Exp '/' Exp			{ Div $1 $3 }
	| int					{ Int $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
	= Plus Exp Exp
	| Minus Exp Exp
	| Times Exp Exp
	| Div Exp Exp
	| Int Int
	deriving (Eq, Show)

}
