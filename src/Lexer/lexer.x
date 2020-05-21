{
{-# LANGUAGE OverloadedStrings                 #-}
module Lexer.Lexer (lexer, Token(..)) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$semi = \;

@bool = (true) | (false)

tokens :-

  $white+                           ;
  "--".*                            ;
  $digit+                           { \s -> TokenInt (read s) }
  @bool                             { \b -> TokenBool (if b == "true" then True else False) }
  $semi                             { \s -> TokenSemicolon }

  \=                                { \s -> TokenAssign }

  \+                                { \s -> TokenPlus }
  \-                                { \s -> TokenMinus }
  \*                                { \s -> TokenTimes }
  \/                                { \s -> TokenDiv }

  \(                                { \s -> TokenLParen }
  \)                                { \s -> TokenRParen }
  \==                               { \s -> TokenEq }
  \>                                { \s -> TokenGT }
  \>=                               { \s -> TokenGE }
  \<                                { \s -> TokenLT }
  \<=                               { \s -> TokenLE }

  \[                                { \s -> TokenLList }
  \]                                { \s -> TokenRList }
  \,                                { \s -> TokenColon }

  if                                { \s -> TokenIf }
  then                              { \s -> TokenThen }
  else                              { \s -> TokenElse }
  $alpha [$alpha $digit \_ \']*     { \s -> TokenVar s }

{

-- The token type:
data Token
  = TokenInt Integer
  | TokenAssign

  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv

  | TokenLParen
  | TokenRParen
  | TokenEq
  | TokenGT
  | TokenGE
  | TokenLT
  | TokenLE

  | TokenLList
  | TokenRList
  | TokenColon

  | TokenIf
  | TokenThen
  | TokenElse
  | TokenVar String
  | TokenBool Bool
  | TokenSemicolon
  deriving (Eq,Show)

lexer = alexScanTokens

}