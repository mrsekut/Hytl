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


%right '='
%left "=>"
%left "if" "then" "else"
%right ':'
%left '(' ')' '[' ']' ','
%nonassoc '>' "==" '<' "<=" ">="
%left '+' '-'
%left '*' '/'

%%

Program :: { Program }
	: Stmt 									{ Program [$1] }
	| Program Stmt 							{ merge $1 $2 }

Stmt :: { Stmt }
	: var '=' Exp							{ Assign $1 $3 }

	| var var '=' Exp						{ Assign $1 (Lambda [PVar $2] $4) }
	| var '(' var ')' '=' Exp				{ Assign $1 (Lambda [PVar $3] $6) }
	| var '[' ']' '=' Exp					{ Assign $1 (Lambda [PList []] $5) }
	| var '(' paramsVarList ')' '=' Exp		{ Assign $1 (Lambda [PList $3] $6) }

	| Exp									{ Exp $1 }


Exp :: { Exp }
	: Exp '+' Exp						{ BinOp Add $1 $3 }
	| Exp '-' Exp						{ BinOp Sub $1 $3 }
	| Exp '*' Exp						{ BinOp Mul $1 $3 }
	| Exp '/' Exp						{ BinOp Div $1 $3 }

	| var Factor 						{ App $1 $2 }

	| Exp "==" Exp						{ BinOp Eq $1 $3 }
	| Exp '>' Exp						{ BinOp Gt $1 $3 }
	| Exp ">=" Exp						{ BinOp Ge $1 $3 }
	| Exp '<' Exp						{ BinOp Lt $1 $3 }
	| Exp "<=" Exp						{ BinOp Le $1 $3 }

	| "if" Exp "then" Exp "else" Exp 	{ If $2 $4 $6 }
	| list								{ List $1 }

	| Factor							{ $1 }

Factor
	: '(' Exp ')'						{ $2 }
	| int								{ Nat $1 }
	| var								{ Var $1 }
	| bool								{ Bool $1 }


{-- List --}

list : '[' elem ']'						{ $2 }
	 | Factor ':' list					{ $1 : $3 }

elem : {- empty -}           	 		{ [] }
     | Exp ',' elem          			{ $1 : $3 }
     | Exp            					{ $1 : [] }


paramsVarList
	: var ':' var 						{ [PVar $1, PVar $3] }
	| var ':' paramsVarList				{ (PVar $1) : $3 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

merge :: Program -> Stmt -> Program
merge (Program xs) x = Program (xs ++ [x])

}
