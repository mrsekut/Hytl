{
module Parser.Parser (parse, Exp(..)) where

import Lexer.Lexer (Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }

%token
	int             { TokenInt $$}
	var				{ TokenVar $$ }
	fn				{ TokenLambda }
	'+'             { TokenPlus }
	'-'             { TokenMinus }
	'*'             { TokenTimes }
	'/'             { TokenDiv }
	'='             { TokenEq }


%right '='
%left '+' '-'
%left '*' '/'

%%

Exp
	: fn var var '=' Exp	{ Lambda $2 $3 $5}
	| Exp '+' Exp			{ Plus $1 $3 }
	| Exp '-' Exp			{ Minus $1 $3 }
	| Exp '*' Exp			{ Times $1 $3 }
	| Exp '/' Exp			{ Div $1 $3 }
	| int					{ Int $1 }
	| var					{ Var $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
	= Plus Exp Exp
	| Minus Exp Exp
	| Times Exp Exp
	| Div Exp Exp
	| Int Int
	| Lambda String String Exp
	| Var String
	deriving (Eq, Show)

}
