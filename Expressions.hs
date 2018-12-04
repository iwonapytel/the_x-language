module Expressions where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class
import Text.Show
import Debug.Trace
import Data.Map as Map
import ErrM
import AbsGrammar
import LexGrammar
import ParGrammar
import PrintGrammar
import StateEnv

--- Expressions semantics

semBinExp :: TypeName -> Exp -> Exp -> Semantics (Val, Val)
semBinExp typename e1 e2 = do
	_e1 <- semExp e1
	_e2 <- semExp e2
	case (_e1, _e2) of
		(INT a, INT b) -> do
			case typename of
				TInt -> return (_e1, _e2)
				_ -> throwError "Incompatible types for arithmetic expression"
		(BOOL a, BOOL b) -> do
			case typename of
				TBool -> return (_e1, _e2)
				_ -> throwError "Incompatible types for boolean expression"

semExp :: Exp -> Semantics Val
semExp ex = do
	case ex of
		(ELitInt i)       -> return (INT (fromInteger i))
		(ELitBool b)      -> case b of
					ETrue ->  return (BOOL True)
					EFalse -> return (BOOL False)
		(EVar ident)      -> getVarVal ident
		(EArr ident expr) -> do
					 val <- semExp expr
					 getArrElem ident val
		(Easgn var op exp2) -> do
					 val <- semExp exp2
					 semAssignmentExp op var val
		(Efunk ident) -> do
					 callFunction ident []
		(Efunkpar ident exprs) -> do
					 vals <- mapM semExp exprs
					 callFunction ident vals
		(Elor exp1 exp2)  -> do
					 (BOOL e1, BOOL e2) <- semBinExp TBool exp1 exp2
					 return (BOOL (e1 || e2))
		(Eland exp1 exp2) -> do
					 (BOOL a, BOOL b) <- semBinExp TBool exp1 exp2
					 return (BOOL (a && b))
		(Eeq exp1 exp2) -> do
					 a <- semExp exp1
					 b <- semExp exp2
				 	 return (BOOL (a == b))
		(Eneq exp1 exp2) -> do
				  	BOOL a <- semExp (Eeq exp1 exp2)
				  	return (BOOL (not a))
		(Elthen exp1 exp2) -> do
				 	(INT a, INT b) <- semBinExp TInt exp1 exp2
				 	return (BOOL (a < b))
		(Egrthen exp1 exp2) -> do
					(INT a, INT b) <- semBinExp TInt exp1 exp2
					return (BOOL (a > b))
		(Ele exp1 exp2) -> do
					(INT a, INT b) <- semBinExp TInt exp1 exp2
					return (BOOL (a <= b))
		(Ege exp1 exp2) -> do
					(INT a, INT b) <- semBinExp TInt exp1 exp2
					return (BOOL (a >= b))
		(Eplus exp1 exp2) -> do
					(INT a, INT b) <- semBinExp TInt exp1 exp2
					return (INT (a + b))
		(Eminus exp1 exp2) -> do
					(INT a, INT b) <- semBinExp TInt exp1 exp2
					return (INT (a - b))
		(Etimes exp1 exp2) -> do
					(INT a, INT b) <- semBinExp TInt exp1 exp2
					return (INT (a * b))
		(Ediv exp1 exp2) -> do
					(INT a, INT b) <- semBinExp TInt exp1 exp2
					if (b == 0) then throwError "Division by zero"
					else return (INT (div a b))
		(EPostInc var) -> do
					semAssignmentExp AsgnAdd var (INT 1)
		(EPostDec var) -> do
					semAssignmentExp AsgnAdd var (INT (-1))
		--(Epreop unary_operator exp) -> do
		--      a <- transExp exp
		--      transUnary_operator unary_operator a
		_      -> throwError "Unsupported expression type"


semAssignmentExp :: AsgnOp -> Exp -> Val -> Semantics Val
semAssignmentExp op var val = do
	case op of
		Asgn    -> semAssignment_ var val
		AsgnAdd -> semAssignmentOp_ (+) var val
		AsgnMul -> semAssignmentOp_ (*) var val
		AsgnSub -> semAssignmentOp_ (-) var val
		AsgnDiv -> case val of
			(INT i) -> if (i == 0) then throwError "Division by zero"
								 else semAssignmentOp_ (div) var val
			_       -> semAssignmentOp_ (div) var val

semAssignment_ :: Exp -> Val -> Semantics Val
semAssignment_ var val = case var of
	EVar ident -> setVarVal ident val
	EArr ident expr -> do
		index <- semExp expr
		setArrElem ident index val
	_       -> throwError "Unsupported yet"

semAssignmentOp_ :: (Int -> Int -> Int) -> Exp -> Val -> Semantics Val
semAssignmentOp_ op var val = do
	oldValue <- semExp var
	case (oldValue, val) of
		(INT i, INT j) -> do
			semAssignment_ var (INT (op i j))
		_       -> throwError "Unsupported assignment type"

semMultipleVals :: [Exp]-> Semantics [Val]
semMultipleVals (e:expr) = do
	 val <- semExp e
	 vals <- semMultipleVals expr
	 return ([val] ++ vals)

semMultipleVals [] = return []
