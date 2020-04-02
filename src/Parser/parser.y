{
module Parser.Parser (parse) where

import Parser.AST
import Lexer.Lexer (Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }

%token
	int				{ TokenInt $$}
	var				{ TokenVar $$ }
	bool			{ TokenBool $$ }

	'='				{ TokenAssign  }
	"=>"			{ TokenLambda }

	'+'				{ TokenPlus }
	'-'				{ TokenMinus }
	'*'				{ TokenTimes }
	'/'				{ TokenDiv }

	'('				{ TokenLParen }
	')'				{ TokenRParen }
	"=="			{ TokenEq }
	'>'				{ TokenGT }
	">="			{ TokenGE }
	'<'				{ TokenLT }
	"<="			{ TokenLE }

	"if"			{ TokenIf }
	"then"			{ TokenThen }
	"else"			{ TokenElse }


%right '='
%left "=>"
%left "if" "then" "else"
%left '>' "==" '<' "<=" ">="
%left '+' '-'
%left '*' '/'

%%

Program :: { Program }
	: Stmt								{ Program [$1] }

Stmt :: { Stmt }
	: var '=' Exp						{ Assign $1 $3 }
	| "if" Exp "then" Exp "else" Exp	{ If $2 $4 $6 }
	| Exp								{ Exp $1 }

Exp :: { Exp }
	: var "=>" Exp						{ Lambda $1 $3 }

	| Exp '+' Exp						{ Add $1 $3 }
	| Exp '-' Exp						{ Sub $1 $3 }
	| Exp '*' Exp						{ Mul $1 $3 }
	| Exp '/' Exp						{ Div $1 $3 }

	| var '(' Exp ')'					{ App $1 $3 }
	| Exp "==" Exp						{ Eq $1 $3 }
	| Exp '>' Exp						{ Gt $1 $3 }
	| Exp ">=" Exp						{ Ge $1 $3 }
	| Exp '<' Exp						{ Lt $1 $3 }
	| Exp "<=" Exp						{ Le $1 $3 }

	| int								{ Nat $1 }
	| var								{ Var $1 }
	| bool								{ Bool $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
