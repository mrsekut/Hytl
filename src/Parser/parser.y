{
module Parser.Parser (parse) where

import Parser.AST (Exp(..))
import Lexer.Lexer (Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }

%token
	int             { TokenInt $$}
	var				{ TokenVar $$ }
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
	: Exp '+' Exp			{ Plus $1 $3 }
	| Exp '-' Exp			{ Minus $1 $3 }
	| Exp '*' Exp			{ Times $1 $3 }
	| Exp '/' Exp			{ Div $1 $3 }
	| var '=' Exp			{ Assign $1 $3 }
	| int					{ Int $1 }
	| var					{ Var $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
