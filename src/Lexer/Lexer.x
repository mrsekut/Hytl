{
{-# LANGUAGE OverloadedStrings                 #-}
module Lexer.Lexer (lexer, Token(..)) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z] -- alphabets

tokens :-

  $white+				                    ;
  "--".*				                    ;
  $digit+				                    { \s -> TokenInt (read s) }
  fn                                { \s -> TokenLambda }
  \=                                { \s -> TokenEq}
  \+                                { \s -> TokenPlus }
  \-                                { \s -> TokenMinus }
  \*                                { \s -> TokenTimes }
  \/                                { \s -> TokenDiv }
  $alpha [$alpha $digit \_ \']*		  { \s -> TokenVar s }

{

-- The token type:
data Token
  = TokenInt Int
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenVar String
  | TokenLambda
  deriving (Eq,Show)

lexer = alexScanTokens

}