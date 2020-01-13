{
{-# LANGUAGE OverloadedStrings                 #-}
module Lexer.Lexer (alexScanTokens, Token(..)) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z] -- alphabets

tokens :-

  $white+				                    ;
  "--".*				                    ;
  $digit+				                    { \s -> TokenInt (read s) }
  \+                                { \s -> TokenPlus }
  \-                                { \s -> TokenMinus }

{

-- The token type:
data Token
  = TokenInt Int
  | TokenPlus
  | TokenMinus
  deriving (Eq,Show)

}