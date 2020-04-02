{-# OPTIONS_GHC -w #-}
module Parser.Parser (parse) where

import Parser.AST
	(Exp(..)
    , Stmt(..)
	)
import Lexer.Lexer (Token(..))
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Stmt)
	| HappyAbsSyn5 (Exp)

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
 action_39 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_17 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,98) ([224,32800,0,1024,0,0,49152,499,0,49152,16,0,57344,0,30720,190,264,1792,0,14,7168,0,56,28672,0,224,49152,1,896,0,7,3584,0,28,14336,0,7680,0,60,30720,0,240,57344,1,0,0,0,3072,0,24,48128,31,15992,61440,124,7,49152,2547,0,14336,0,40448,15,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Stmt","Exp","int","var","bool","'='","\"=>\"","'+'","'-'","'*'","'/'","'('","')'","\"==\"","'>'","\">=\"","'<'","\"<=\"","\"if\"","\"then\"","\"else\"","%eof"]
        bit_start = st * 25
        bit_end = (st + 1) * 25
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..24]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (6) = happyShift action_5
action_0 (7) = happyShift action_6
action_0 (8) = happyShift action_7
action_0 (22) = happyShift action_8
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (7) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (9) = happyShift action_11
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (25) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (11) = happyShift action_14
action_4 (12) = happyShift action_15
action_4 (13) = happyShift action_16
action_4 (14) = happyShift action_17
action_4 (17) = happyShift action_18
action_4 (18) = happyShift action_19
action_4 (19) = happyShift action_20
action_4 (20) = happyShift action_21
action_4 (21) = happyShift action_22
action_4 _ = happyReduce_3

action_5 _ = happyReduce_15

action_6 (9) = happyShift action_11
action_6 (10) = happyShift action_12
action_6 (15) = happyShift action_13
action_6 _ = happyReduce_16

action_7 _ = happyReduce_17

action_8 (6) = happyShift action_5
action_8 (7) = happyShift action_10
action_8 (8) = happyShift action_7
action_8 (5) = happyGoto action_9
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (11) = happyShift action_14
action_9 (12) = happyShift action_15
action_9 (13) = happyShift action_16
action_9 (14) = happyShift action_17
action_9 (17) = happyShift action_18
action_9 (18) = happyShift action_19
action_9 (19) = happyShift action_20
action_9 (20) = happyShift action_21
action_9 (21) = happyShift action_22
action_9 (23) = happyShift action_35
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (10) = happyShift action_12
action_10 (15) = happyShift action_13
action_10 _ = happyReduce_16

action_11 (6) = happyShift action_5
action_11 (7) = happyShift action_10
action_11 (8) = happyShift action_7
action_11 (5) = happyGoto action_34
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (6) = happyShift action_5
action_12 (7) = happyShift action_10
action_12 (8) = happyShift action_7
action_12 (5) = happyGoto action_33
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (6) = happyShift action_5
action_13 (7) = happyShift action_10
action_13 (8) = happyShift action_7
action_13 (5) = happyGoto action_32
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (6) = happyShift action_5
action_14 (7) = happyShift action_10
action_14 (8) = happyShift action_7
action_14 (5) = happyGoto action_31
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (6) = happyShift action_5
action_15 (7) = happyShift action_10
action_15 (8) = happyShift action_7
action_15 (5) = happyGoto action_30
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (6) = happyShift action_5
action_16 (7) = happyShift action_10
action_16 (8) = happyShift action_7
action_16 (5) = happyGoto action_29
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (6) = happyShift action_5
action_17 (7) = happyShift action_10
action_17 (8) = happyShift action_7
action_17 (5) = happyGoto action_28
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (6) = happyShift action_5
action_18 (7) = happyShift action_10
action_18 (8) = happyShift action_7
action_18 (5) = happyGoto action_27
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (6) = happyShift action_5
action_19 (7) = happyShift action_10
action_19 (8) = happyShift action_7
action_19 (5) = happyGoto action_26
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (6) = happyShift action_5
action_20 (7) = happyShift action_10
action_20 (8) = happyShift action_7
action_20 (5) = happyGoto action_25
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (6) = happyShift action_5
action_21 (7) = happyShift action_10
action_21 (8) = happyShift action_7
action_21 (5) = happyGoto action_24
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (6) = happyShift action_5
action_22 (7) = happyShift action_10
action_22 (8) = happyShift action_7
action_22 (5) = happyGoto action_23
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (11) = happyShift action_14
action_23 (12) = happyShift action_15
action_23 (13) = happyShift action_16
action_23 (14) = happyShift action_17
action_23 _ = happyReduce_14

action_24 (11) = happyShift action_14
action_24 (12) = happyShift action_15
action_24 (13) = happyShift action_16
action_24 (14) = happyShift action_17
action_24 _ = happyReduce_13

action_25 (11) = happyShift action_14
action_25 (12) = happyShift action_15
action_25 (13) = happyShift action_16
action_25 (14) = happyShift action_17
action_25 _ = happyReduce_12

action_26 (11) = happyShift action_14
action_26 (12) = happyShift action_15
action_26 (13) = happyShift action_16
action_26 (14) = happyShift action_17
action_26 _ = happyReduce_11

action_27 (11) = happyShift action_14
action_27 (12) = happyShift action_15
action_27 (13) = happyShift action_16
action_27 (14) = happyShift action_17
action_27 _ = happyReduce_10

action_28 _ = happyReduce_8

action_29 _ = happyReduce_7

action_30 (13) = happyShift action_16
action_30 (14) = happyShift action_17
action_30 _ = happyReduce_6

action_31 (13) = happyShift action_16
action_31 (14) = happyShift action_17
action_31 _ = happyReduce_5

action_32 (11) = happyShift action_14
action_32 (12) = happyShift action_15
action_32 (13) = happyShift action_16
action_32 (14) = happyShift action_17
action_32 (16) = happyShift action_37
action_32 (17) = happyShift action_18
action_32 (18) = happyShift action_19
action_32 (19) = happyShift action_20
action_32 (20) = happyShift action_21
action_32 (21) = happyShift action_22
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (11) = happyShift action_14
action_33 (12) = happyShift action_15
action_33 (13) = happyShift action_16
action_33 (14) = happyShift action_17
action_33 (17) = happyShift action_18
action_33 (18) = happyShift action_19
action_33 (19) = happyShift action_20
action_33 (20) = happyShift action_21
action_33 (21) = happyShift action_22
action_33 _ = happyReduce_4

action_34 (11) = happyShift action_14
action_34 (12) = happyShift action_15
action_34 (13) = happyShift action_16
action_34 (14) = happyShift action_17
action_34 (17) = happyShift action_18
action_34 (18) = happyShift action_19
action_34 (19) = happyShift action_20
action_34 (20) = happyShift action_21
action_34 (21) = happyShift action_22
action_34 _ = happyReduce_1

action_35 (6) = happyShift action_5
action_35 (7) = happyShift action_10
action_35 (8) = happyShift action_7
action_35 (5) = happyGoto action_36
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (11) = happyShift action_14
action_36 (12) = happyShift action_15
action_36 (13) = happyShift action_16
action_36 (14) = happyShift action_17
action_36 (17) = happyShift action_18
action_36 (18) = happyShift action_19
action_36 (19) = happyShift action_20
action_36 (20) = happyShift action_21
action_36 (21) = happyShift action_22
action_36 (24) = happyShift action_38
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_9

action_38 (6) = happyShift action_5
action_38 (7) = happyShift action_10
action_38 (8) = happyShift action_7
action_38 (5) = happyGoto action_39
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (11) = happyShift action_14
action_39 (12) = happyShift action_15
action_39 (13) = happyShift action_16
action_39 (14) = happyShift action_17
action_39 (17) = happyShift action_18
action_39 (18) = happyShift action_19
action_39 (19) = happyShift action_20
action_39 (20) = happyShift action_21
action_39 (21) = happyShift action_22
action_39 _ = happyReduce_2

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happyReduce 6 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Exp happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn5
		 (Lambda happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Add happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Div happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 5 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (App happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Gt happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Ge happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Le happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  5 happyReduction_15
happyReduction_15 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn5
		 (Nat happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  5 happyReduction_16
happyReduction_16 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn5
		 (Var happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  5 happyReduction_17
happyReduction_17 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn5
		 (Bool happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 25 25 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 6;
	TokenVar happy_dollar_dollar -> cont 7;
	TokenBool happy_dollar_dollar -> cont 8;
	TokenAssign -> cont 9;
	TokenLambda -> cont 10;
	TokenPlus -> cont 11;
	TokenMinus -> cont 12;
	TokenTimes -> cont 13;
	TokenDiv -> cont 14;
	TokenLParen -> cont 15;
	TokenRParen -> cont 16;
	TokenEq -> cont 17;
	TokenGT -> cont 18;
	TokenGE -> cont 19;
	TokenLT -> cont 20;
	TokenLE -> cont 21;
	TokenIf -> cont 22;
	TokenThen -> cont 23;
	TokenElse -> cont 24;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 25 tk tks = happyError' (tks, explist)
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
