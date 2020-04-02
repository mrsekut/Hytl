{-# OPTIONS_GHC -w #-}
module Parser.Parser (parse) where

import Parser.AST
import Lexer.Lexer (Token(..))
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Program)
	| HappyAbsSyn5 (Stmt)
	| HappyAbsSyn6 (Exp)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,105) ([448,64,14,2,32768,61440,124,0,0,67,0,0,56,49152,16385,0,4096,40448,47,264,7168,0,224,0,7,14336,0,448,0,14,28672,0,896,0,28,57344,0,1792,0,56,0,0,49152,3,7680,0,240,32768,7,15360,0,0,0,0,24576,0,768,0,4062,61440,124,59264,57347,0,0,0,52992,39,0,3584,0,40448,15,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Program","Stmt","Exp","int","var","bool","'='","\"=>\"","'+'","'-'","'*'","'/'","'('","')'","\"==\"","'>'","\">=\"","'<'","\"<=\"","\"if\"","\"then\"","\"else\"","semi","%eof"]
        bit_start = st * 27
        bit_end = (st + 1) * 27
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..26]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (7) = happyShift action_4
action_0 (8) = happyShift action_5
action_0 (9) = happyShift action_6
action_0 (23) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (7) = happyShift action_4
action_1 (8) = happyShift action_5
action_1 (9) = happyShift action_6
action_1 (23) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (26) = happyShift action_24
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (12) = happyShift action_15
action_3 (13) = happyShift action_16
action_3 (14) = happyShift action_17
action_3 (15) = happyShift action_18
action_3 (18) = happyShift action_19
action_3 (19) = happyShift action_20
action_3 (20) = happyShift action_21
action_3 (21) = happyShift action_22
action_3 (22) = happyShift action_23
action_3 _ = happyReduce_5

action_4 _ = happyReduce_17

action_5 (10) = happyShift action_12
action_5 (11) = happyShift action_13
action_5 (16) = happyShift action_14
action_5 _ = happyReduce_18

action_6 _ = happyReduce_19

action_7 (7) = happyShift action_4
action_7 (8) = happyShift action_11
action_7 (9) = happyShift action_6
action_7 (6) = happyGoto action_10
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (7) = happyShift action_4
action_8 (8) = happyShift action_5
action_8 (9) = happyShift action_6
action_8 (23) = happyShift action_7
action_8 (27) = happyAccept
action_8 (5) = happyGoto action_9
action_8 (6) = happyGoto action_3
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (26) = happyShift action_38
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (12) = happyShift action_15
action_10 (13) = happyShift action_16
action_10 (14) = happyShift action_17
action_10 (15) = happyShift action_18
action_10 (18) = happyShift action_19
action_10 (19) = happyShift action_20
action_10 (20) = happyShift action_21
action_10 (21) = happyShift action_22
action_10 (22) = happyShift action_23
action_10 (24) = happyShift action_37
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (11) = happyShift action_13
action_11 (16) = happyShift action_14
action_11 _ = happyReduce_18

action_12 (7) = happyShift action_4
action_12 (8) = happyShift action_11
action_12 (9) = happyShift action_6
action_12 (6) = happyGoto action_36
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (7) = happyShift action_4
action_13 (8) = happyShift action_11
action_13 (9) = happyShift action_6
action_13 (6) = happyGoto action_35
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (7) = happyShift action_4
action_14 (8) = happyShift action_11
action_14 (9) = happyShift action_6
action_14 (6) = happyGoto action_34
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (7) = happyShift action_4
action_15 (8) = happyShift action_11
action_15 (9) = happyShift action_6
action_15 (6) = happyGoto action_33
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (7) = happyShift action_4
action_16 (8) = happyShift action_11
action_16 (9) = happyShift action_6
action_16 (6) = happyGoto action_32
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (7) = happyShift action_4
action_17 (8) = happyShift action_11
action_17 (9) = happyShift action_6
action_17 (6) = happyGoto action_31
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (7) = happyShift action_4
action_18 (8) = happyShift action_11
action_18 (9) = happyShift action_6
action_18 (6) = happyGoto action_30
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (7) = happyShift action_4
action_19 (8) = happyShift action_11
action_19 (9) = happyShift action_6
action_19 (6) = happyGoto action_29
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (7) = happyShift action_4
action_20 (8) = happyShift action_11
action_20 (9) = happyShift action_6
action_20 (6) = happyGoto action_28
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (7) = happyShift action_4
action_21 (8) = happyShift action_11
action_21 (9) = happyShift action_6
action_21 (6) = happyGoto action_27
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (7) = happyShift action_4
action_22 (8) = happyShift action_11
action_22 (9) = happyShift action_6
action_22 (6) = happyGoto action_26
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (7) = happyShift action_4
action_23 (8) = happyShift action_11
action_23 (9) = happyShift action_6
action_23 (6) = happyGoto action_25
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_1

action_25 (12) = happyShift action_15
action_25 (13) = happyShift action_16
action_25 (14) = happyShift action_17
action_25 (15) = happyShift action_18
action_25 _ = happyReduce_16

action_26 (12) = happyShift action_15
action_26 (13) = happyShift action_16
action_26 (14) = happyShift action_17
action_26 (15) = happyShift action_18
action_26 _ = happyReduce_15

action_27 (12) = happyShift action_15
action_27 (13) = happyShift action_16
action_27 (14) = happyShift action_17
action_27 (15) = happyShift action_18
action_27 _ = happyReduce_14

action_28 (12) = happyShift action_15
action_28 (13) = happyShift action_16
action_28 (14) = happyShift action_17
action_28 (15) = happyShift action_18
action_28 _ = happyReduce_13

action_29 (12) = happyShift action_15
action_29 (13) = happyShift action_16
action_29 (14) = happyShift action_17
action_29 (15) = happyShift action_18
action_29 _ = happyReduce_12

action_30 _ = happyReduce_10

action_31 _ = happyReduce_9

action_32 (14) = happyShift action_17
action_32 (15) = happyShift action_18
action_32 _ = happyReduce_8

action_33 (14) = happyShift action_17
action_33 (15) = happyShift action_18
action_33 _ = happyReduce_7

action_34 (12) = happyShift action_15
action_34 (13) = happyShift action_16
action_34 (14) = happyShift action_17
action_34 (15) = happyShift action_18
action_34 (17) = happyShift action_40
action_34 (18) = happyShift action_19
action_34 (19) = happyShift action_20
action_34 (20) = happyShift action_21
action_34 (21) = happyShift action_22
action_34 (22) = happyShift action_23
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (12) = happyShift action_15
action_35 (13) = happyShift action_16
action_35 (14) = happyShift action_17
action_35 (15) = happyShift action_18
action_35 (18) = happyShift action_19
action_35 (19) = happyShift action_20
action_35 (20) = happyShift action_21
action_35 (21) = happyShift action_22
action_35 (22) = happyShift action_23
action_35 _ = happyReduce_6

action_36 (12) = happyShift action_15
action_36 (13) = happyShift action_16
action_36 (14) = happyShift action_17
action_36 (15) = happyShift action_18
action_36 (18) = happyShift action_19
action_36 (19) = happyShift action_20
action_36 (20) = happyShift action_21
action_36 (21) = happyShift action_22
action_36 (22) = happyShift action_23
action_36 _ = happyReduce_3

action_37 (7) = happyShift action_4
action_37 (8) = happyShift action_11
action_37 (9) = happyShift action_6
action_37 (6) = happyGoto action_39
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_2

action_39 (12) = happyShift action_15
action_39 (13) = happyShift action_16
action_39 (14) = happyShift action_17
action_39 (15) = happyShift action_18
action_39 (18) = happyShift action_19
action_39 (19) = happyShift action_20
action_39 (20) = happyShift action_21
action_39 (21) = happyShift action_22
action_39 (22) = happyShift action_23
action_39 (25) = happyShift action_41
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_11

action_41 (7) = happyShift action_4
action_41 (8) = happyShift action_11
action_41 (9) = happyShift action_6
action_41 (6) = happyGoto action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (12) = happyShift action_15
action_42 (13) = happyShift action_16
action_42 (14) = happyShift action_17
action_42 (15) = happyShift action_18
action_42 (18) = happyShift action_19
action_42 (19) = happyShift action_20
action_42 (20) = happyShift action_21
action_42 (21) = happyShift action_22
action_42 (22) = happyShift action_23
action_42 _ = happyReduce_4

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Program [happy_var_1]
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (merge happy_var_1 happy_var_2
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn5
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 6 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Exp happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn6
		 (Lambda happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Add happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6 happyReduction_10
happyReduction_10 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Div happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 4 6 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (App happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  6 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  6 happyReduction_13
happyReduction_13 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Gt happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  6 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Ge happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  6 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  6 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Le happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  6 happyReduction_17
happyReduction_17 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn6
		 (Nat happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  6 happyReduction_18
happyReduction_18 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn6
		 (Var happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  6 happyReduction_19
happyReduction_19 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn6
		 (Bool happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 27 27 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 7;
	TokenVar happy_dollar_dollar -> cont 8;
	TokenBool happy_dollar_dollar -> cont 9;
	TokenAssign -> cont 10;
	TokenLambda -> cont 11;
	TokenPlus -> cont 12;
	TokenMinus -> cont 13;
	TokenTimes -> cont 14;
	TokenDiv -> cont 15;
	TokenLParen -> cont 16;
	TokenRParen -> cont 17;
	TokenEq -> cont 18;
	TokenGT -> cont 19;
	TokenGE -> cont 20;
	TokenLT -> cont 21;
	TokenLE -> cont 22;
	TokenIf -> cont 23;
	TokenThen -> cont 24;
	TokenElse -> cont 25;
	TokenSemicolon -> cont 26;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 27 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

merge :: Program -> Stmt -> Program
merge (Program xs) x = Program (xs ++ [x])
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
