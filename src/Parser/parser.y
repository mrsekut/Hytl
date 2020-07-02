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

  	'[' 			{ TokenLList }
  	']' 			{ TokenRList }
  	',' 			{ TokenComma }
  	':' 			{ TokenColon }

	"if"			{ TokenIf }
	"then"			{ TokenThen }
	"else"			{ TokenElse }
	semi			{ TokenSemicolon }


%left semi
%right '='
%left "=>"
%left "if" "then" "else"
%left '[' ']' ',' ':'
%left '>' "==" '<' "<=" ">="
%left '+' '-'
%left '*' '/'

%%

Program :: { Program }
	: Stmt semi							{ Program [$1] }
	| Program Stmt semi					{ merge $1 $2 }

Stmt :: { Stmt }
	: var '=' Exp 						{ Assign $1 $3 }
	| var var '=' Exp					{ Assign $1 (Lambda (OneArg $2) $4) }
	| var strList '=' Exp				{ Assign $1 (Lambda (MultArgs $2) $4) }
	| Exp 								{ Exp $1 }

Exp :: { Exp }
	: Exp '+' Exp						{ BinOp Add $1 $3 }
	| Exp '-' Exp						{ BinOp Sub $1 $3 }
	| Exp '*' Exp						{ BinOp Mul $1 $3 }
	| Exp '/' Exp						{ BinOp Div $1 $3 }

	| var Exp 							{ App $1 $2 }
	| var '(' Exp ')'					{ App $1 $3 }
	| Exp "==" Exp						{ BinOp Eq $1 $3 }
	| Exp '>' Exp						{ BinOp Gt $1 $3 }
	| Exp ">=" Exp						{ BinOp Ge $1 $3 }
	| Exp '<' Exp						{ BinOp Lt $1 $3 }
	| Exp "<=" Exp						{ BinOp Le $1 $3 }

	| list								{ List $1 }

	| "if" Exp "then" Exp "else" Exp 	{ If $2 $4 $6 }

	| int								{ Nat $1 }
	| var								{ Var $1 }
	| bool								{ Bool $1 }


list : '[' elem ']'						{ $2 }
	 | Exp ':' list						{ $1 : $3 }


elem : {- empty -}           	 		{ [] }
     | Exp ',' elem           			{ $1 : $3 }
     | Exp            					{ $1 : [] }


strList : '[' strElem ']'				{ $2 }
	 | var ':' strList					{ $1 : $3 }

strElem : {- empty -}           	 	{ [] }
     | var ',' strElem           		{ $1 : $3 }
     | var            					{ $1 : [] }



{

parseError :: [Token] -> a
parseError _ = error "Parse error"

merge :: Program -> Stmt -> Program
merge (Program xs) x = Program (xs ++ [x])

}
