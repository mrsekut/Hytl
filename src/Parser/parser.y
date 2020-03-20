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
	bool			{ TokenBool $$ }
	'+'				{ TokenPlus }
	'-'				{ TokenMinus }
	'*'				{ TokenTimes }
	'/'				{ TokenDiv }
	'='				{ TokenAssign  }
	"=>"			{ TokenLambda }
	"=="			{ TokenEq }
	'('				{ TokenLParen }
	')'				{ TokenRParen }
	"if"			{ TokenIf }
	"then"			{ TokenThen }
	"else"			{ TokenElse }
	'>'				{ TokenGT }


%right '='
%left "=>"
%left "if" "then" "else"
%left '>' "=="
%left '+' '-'
%left '*' '/'

%%

Exp
	: var '=' Exp						{ Assign $1 $3 }
	| var '(' Exp ')'					{ App $1 $3 }
	| "if" Exp "then" Exp "else" Exp	{ If $2 $4 $6 }
	| Exp '+' Exp						{ Add $1 $3 }
	| Exp '-' Exp						{ Sub $1 $3 }
	| Exp '*' Exp						{ Mul $1 $3 }
	| Exp '/' Exp						{ Div $1 $3 }
	| Exp '>' Exp						{ Gt $1 $3 }
	| var "=>" Exp						{ Lambda $1 $3 }
	| Exp "==" Exp						{ Eq $1 $3 }
	| int								{ Nat $1 }
	| var								{ Var $1 }
	| bool								{ Bool $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
