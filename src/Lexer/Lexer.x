{
{-# LANGUAGE OverloadedStrings                 #-}
module Lexer.Lexer (lexer, Token(..)) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

@bool = (true) | (false)

tokens :-

  $white+                           ;
  "--".*                            ;
  $digit+                           { \s -> TokenInt (read s) }
  @bool                             { \b -> TokenBool (if b == "true" then True else False) }
  \=                                { \s -> TokenEq}
  \+                                { \s -> TokenPlus }
  \-                                { \s -> TokenMinus }
  \*                                { \s -> TokenTimes }
  \/                                { \s -> TokenDiv }
  \=>                               { \s -> TokenLambda }
  \(                                { \s -> TokenLParen }
  \)                                { \s -> TokenRParen }
  \>                                { \s -> TokenGT }
  if                                { \s -> TokenIf }
  then                              { \s -> TokenThen }
  else                              { \s -> TokenElse }
  $alpha [$alpha $digit \_ \']*     { \s -> TokenVar s }

{

-- The token type:
data Token
  = TokenInt Integer
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenLambda
  | TokenLParen
  | TokenRParen
  | TokenGT
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenVar String
  | TokenBool Bool
  deriving (Eq,Show)

lexer = alexScanTokens

}