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
  \=                                { \s -> TokenEq}
  \+                                { \s -> TokenPlus }
  \-                                { \s -> TokenMinus }
  \*                                { \s -> TokenTimes }
  \/                                { \s -> TokenDiv }

{

-- The token type:
data Token
  = TokenInt Int
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  deriving (Eq,Show)

lexer = alexScanTokens

}