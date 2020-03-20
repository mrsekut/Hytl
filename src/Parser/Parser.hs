{-# OPTIONS_GHC -w #-}
module Parser.Parser (parse) where

import Parser.AST (Exp(..))
import Lexer.Lexer (Token(..))
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,100) ([112,8208,0,128,0,3998,0,32768,33,0,28672,4096,40448,28719,4096,112,28688,4096,112,28688,4096,112,28688,4096,112,28688,4096,112,28688,4096,112,16,30,7680,0,30,7680,0,30,0,0,0,6144,0,24,56832,15,3998,40448,28687,4096,40448,79,0,112,16,3998,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Exp","int","var","bool","'='","\"=>\"","'+'","'-'","'*'","'/'","'('","')'","\"==\"","'>'","\">=\"","'<'","\"<=\"","\"if\"","\"then\"","\"else\"","%eof"]
        bit_start = st * 24
        bit_end = (st + 1) * 24
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..23]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (5) = happyShift action_4
action_0 (6) = happyShift action_5
action_0 (7) = happyShift action_6
action_0 (21) = happyShift action_7
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (6) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (8) = happyShift action_9
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (10) = happyShift action_12
action_3 (11) = happyShift action_13
action_3 (12) = happyShift action_14
action_3 (13) = happyShift action_15
action_3 (16) = happyShift action_16
action_3 (17) = happyShift action_17
action_3 (18) = happyShift action_18
action_3 (19) = happyShift action_19
action_3 (20) = happyShift action_20
action_3 (24) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_14

action_5 (8) = happyShift action_9
action_5 (9) = happyShift action_10
action_5 (14) = happyShift action_11
action_5 _ = happyReduce_15

action_6 _ = happyReduce_16

action_7 (5) = happyShift action_4
action_7 (6) = happyShift action_5
action_7 (7) = happyShift action_6
action_7 (21) = happyShift action_7
action_7 (4) = happyGoto action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (10) = happyShift action_12
action_8 (11) = happyShift action_13
action_8 (12) = happyShift action_14
action_8 (13) = happyShift action_15
action_8 (16) = happyShift action_16
action_8 (17) = happyShift action_17
action_8 (18) = happyShift action_18
action_8 (19) = happyShift action_19
action_8 (20) = happyShift action_20
action_8 (22) = happyShift action_33
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (5) = happyShift action_4
action_9 (6) = happyShift action_5
action_9 (7) = happyShift action_6
action_9 (21) = happyShift action_7
action_9 (4) = happyGoto action_32
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (5) = happyShift action_4
action_10 (6) = happyShift action_5
action_10 (7) = happyShift action_6
action_10 (21) = happyShift action_7
action_10 (4) = happyGoto action_31
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (5) = happyShift action_4
action_11 (6) = happyShift action_5
action_11 (7) = happyShift action_6
action_11 (21) = happyShift action_7
action_11 (4) = happyGoto action_30
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (5) = happyShift action_4
action_12 (6) = happyShift action_5
action_12 (7) = happyShift action_6
action_12 (21) = happyShift action_7
action_12 (4) = happyGoto action_29
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (5) = happyShift action_4
action_13 (6) = happyShift action_5
action_13 (7) = happyShift action_6
action_13 (21) = happyShift action_7
action_13 (4) = happyGoto action_28
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (5) = happyShift action_4
action_14 (6) = happyShift action_5
action_14 (7) = happyShift action_6
action_14 (21) = happyShift action_7
action_14 (4) = happyGoto action_27
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (5) = happyShift action_4
action_15 (6) = happyShift action_5
action_15 (7) = happyShift action_6
action_15 (21) = happyShift action_7
action_15 (4) = happyGoto action_26
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (5) = happyShift action_4
action_16 (6) = happyShift action_5
action_16 (7) = happyShift action_6
action_16 (21) = happyShift action_7
action_16 (4) = happyGoto action_25
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (5) = happyShift action_4
action_17 (6) = happyShift action_5
action_17 (7) = happyShift action_6
action_17 (21) = happyShift action_7
action_17 (4) = happyGoto action_24
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (5) = happyShift action_4
action_18 (6) = happyShift action_5
action_18 (7) = happyShift action_6
action_18 (21) = happyShift action_7
action_18 (4) = happyGoto action_23
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (5) = happyShift action_4
action_19 (6) = happyShift action_5
action_19 (7) = happyShift action_6
action_19 (21) = happyShift action_7
action_19 (4) = happyGoto action_22
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (5) = happyShift action_4
action_20 (6) = happyShift action_5
action_20 (7) = happyShift action_6
action_20 (21) = happyShift action_7
action_20 (4) = happyGoto action_21
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (10) = happyShift action_12
action_21 (11) = happyShift action_13
action_21 (12) = happyShift action_14
action_21 (13) = happyShift action_15
action_21 _ = happyReduce_12

action_22 (10) = happyShift action_12
action_22 (11) = happyShift action_13
action_22 (12) = happyShift action_14
action_22 (13) = happyShift action_15
action_22 _ = happyReduce_11

action_23 (10) = happyShift action_12
action_23 (11) = happyShift action_13
action_23 (12) = happyShift action_14
action_23 (13) = happyShift action_15
action_23 _ = happyReduce_10

action_24 (10) = happyShift action_12
action_24 (11) = happyShift action_13
action_24 (12) = happyShift action_14
action_24 (13) = happyShift action_15
action_24 _ = happyReduce_9

action_25 (10) = happyShift action_12
action_25 (11) = happyShift action_13
action_25 (12) = happyShift action_14
action_25 (13) = happyShift action_15
action_25 _ = happyReduce_8

action_26 _ = happyReduce_6

action_27 _ = happyReduce_5

action_28 (12) = happyShift action_14
action_28 (13) = happyShift action_15
action_28 _ = happyReduce_4

action_29 (12) = happyShift action_14
action_29 (13) = happyShift action_15
action_29 _ = happyReduce_3

action_30 (10) = happyShift action_12
action_30 (11) = happyShift action_13
action_30 (12) = happyShift action_14
action_30 (13) = happyShift action_15
action_30 (15) = happyShift action_35
action_30 (16) = happyShift action_16
action_30 (17) = happyShift action_17
action_30 (18) = happyShift action_18
action_30 (19) = happyShift action_19
action_30 (20) = happyShift action_20
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (10) = happyShift action_12
action_31 (11) = happyShift action_13
action_31 (12) = happyShift action_14
action_31 (13) = happyShift action_15
action_31 (16) = happyShift action_16
action_31 (17) = happyShift action_17
action_31 (18) = happyShift action_18
action_31 (19) = happyShift action_19
action_31 (20) = happyShift action_20
action_31 _ = happyReduce_2

action_32 (10) = happyShift action_12
action_32 (11) = happyShift action_13
action_32 (12) = happyShift action_14
action_32 (13) = happyShift action_15
action_32 (16) = happyShift action_16
action_32 (17) = happyShift action_17
action_32 (18) = happyShift action_18
action_32 (19) = happyShift action_19
action_32 (20) = happyShift action_20
action_32 _ = happyReduce_1

action_33 (5) = happyShift action_4
action_33 (6) = happyShift action_5
action_33 (7) = happyShift action_6
action_33 (21) = happyShift action_7
action_33 (4) = happyGoto action_34
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (10) = happyShift action_12
action_34 (11) = happyShift action_13
action_34 (12) = happyShift action_14
action_34 (13) = happyShift action_15
action_34 (16) = happyShift action_16
action_34 (17) = happyShift action_17
action_34 (18) = happyShift action_18
action_34 (19) = happyShift action_19
action_34 (20) = happyShift action_20
action_34 (23) = happyShift action_36
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_7

action_36 (5) = happyShift action_4
action_36 (6) = happyShift action_5
action_36 (7) = happyShift action_6
action_36 (21) = happyShift action_7
action_36 (4) = happyGoto action_37
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (10) = happyShift action_12
action_37 (11) = happyShift action_13
action_37 (12) = happyShift action_14
action_37 (13) = happyShift action_15
action_37 (16) = happyShift action_16
action_37 (17) = happyShift action_17
action_37 (18) = happyShift action_18
action_37 (19) = happyShift action_19
action_37 (20) = happyShift action_20
action_37 _ = happyReduce_13

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Lambda happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Add happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Div happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 4 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (App happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Gt happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Ge happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  4 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Le happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 6 4 happyReduction_13
happyReduction_13 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_1  4 happyReduction_14
happyReduction_14 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn4
		 (Nat happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  4 happyReduction_15
happyReduction_15 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  4 happyReduction_16
happyReduction_16 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn4
		 (Bool happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 24 24 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 5;
	TokenVar happy_dollar_dollar -> cont 6;
	TokenBool happy_dollar_dollar -> cont 7;
	TokenAssign -> cont 8;
	TokenLambda -> cont 9;
	TokenPlus -> cont 10;
	TokenMinus -> cont 11;
	TokenTimes -> cont 12;
	TokenDiv -> cont 13;
	TokenLParen -> cont 14;
	TokenRParen -> cont 15;
	TokenEq -> cont 16;
	TokenGT -> cont 17;
	TokenGE -> cont 18;
	TokenLT -> cont 19;
	TokenLE -> cont 20;
	TokenIf -> cont 21;
	TokenThen -> cont 22;
	TokenElse -> cont 23;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 24 tk tks = happyError' (tks, explist)
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
