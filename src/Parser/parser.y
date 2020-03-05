{
module Parser.Parser (parse) where

import Parser.AST (Exp(..))
import Lexer.Lexer (Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }

%token
	int				{ TokenInt $$}
	var				{ TokenVar $$ }
	'+'				{ TokenPlus }
	'-'				{ TokenMinus }
	'*'				{ TokenTimes }
	'/'				{ TokenDiv }
	'='				{ TokenEq }
	"=>"			{ TokenLambda }
	'('				{ TokenLParen }
	')'				{ TokenRParen }


%right '='
%left "=>"
%left '+' '-'
%left '*' '/'

%%

Exp
	: var '=' Exp			{ Assign $1 $3 }
	| var '(' Exp ')'		{ Call $1 $3 }
	| Exp1					{ $1 }


Exp1
	: Exp1 '+' Exp1			{ Plus $1 $3 }
	| Exp1 '-' Exp1			{ Minus $1 $3 }
	| Exp1 '*' Exp1			{ Times $1 $3 }
	| Exp1 '/' Exp1			{ Div $1 $3 }
	| var "=>" Exp1			{ Lambda $1 $3 }
	| int					{ Int $1 }
	| var					{ Var $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
