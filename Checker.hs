module Checker where

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
import Expressions
import Semantic

-- Static checker
checkInstructions :: [Instruction] -> Semantics (Env, [Ret])
checkInstructions (x:xs) = do
	traceShowM x
	(env, ret) <- checkInstruction x
	(env2, ret2) <- local (const env) (checkInstructions xs)
	return (env2, ret ++ ret2)

checkInstructions [] = do
	env <- ask
	return (env, [])

checkInstruction :: Instruction -> Semantics (Env, [Ret])
checkInstruction i = do
	env <- ask
	case i of
		StmInstruction s -> do
			ret <- checkStatement s
			return (env, ret)
		DecInstruction d -> do
			env2 <- checkDeclaration d
			return (env2, [])
		ExpInstruction e -> do
			semExp e
			return (env, [])
		BlockInstruction b -> do
			ret <- checkBlock b
			return (env, ret)

checkBlock :: Block -> Semantics [Ret]
checkBlock b = do
	case b of
		BBlock i -> do
		 (_, ret) <- checkInstructions i
		 return ret
		EmptyBlock -> return []

-- Statements semantic
checkStatement :: Stm -> Semantics [Ret]
checkStatement stm = case stm of
		EmptyStm   -> return []
		PrintStm x -> return []
		SelectStm x -> case x of
			IfStm e i       -> checkIfElseStm e i (StmInstruction EmptyStm)
			IfElseStm e i1 i2 -> checkIfElseStm e i1 i2
		ItStm x -> checkItStm x
		RStm x -> case x of
			ContStm -> return [Continue]
			BreakStm -> return [Break]
			RetExpStm e -> do
				val <- semExp e
				return [Return val]

checkItStm :: IterStm -> Semantics [Ret]
checkItStm stm = do
	case stm of
		WhileStm e i -> checkWhileStm e i
		ForStmPrecond prefor cond postfor instr -> do
			let postforInstr = (ExpInstruction postfor)
			let newInstr = (BlockInstruction (BBlock [instr, postforInstr]))
			_ <- semExp prefor
			checkWhileStm cond newInstr
		ForStm cond postfor instr -> do
			let postforInstr = (ExpInstruction postfor)
			let newInstr = (BlockInstruction (BBlock [instr, postforInstr]))
			checkWhileStm cond newInstr

checkIfElseStm :: Exp -> Instruction -> Instruction -> Semantics [Ret]
checkIfElseStm e i1 i2 = do
	-- check condition type
	val <- semExp e
	cond <- valToCondition val
	-- check all possible branches
	(_, ret1) <- checkInstruction i1
	(_, ret2) <- checkInstruction i2
	return (ret1 ++ ret2)

checkWhileStm :: Exp -> Instruction -> Semantics [Ret]
checkWhileStm e i = do
	-- check condition type
	val <- semExp e
	cond <- valToCondition val
	(_, ret) <- checkInstruction i
	-- filter out breaks and continues, caught by while statement
	let isReturn = \x -> case x of
		Return v -> True
		_ -> False
	return (Prelude.filter isReturn ret)

-- Declarations semantics
checkDeclaration :: Dec -> Semantics Env
checkDeclaration dec = do
	case dec of
		VarDec typename item -> semVarDec typename item
		FuncDec typename func -> checkFunDec typename func

-- static check of function return type
checkFunDec :: TypeName -> Func -> Semantics Env
checkFunDec typename func = do
	case func of
		FunctionNoParams ident block ->
			checkFunDec typename (FunctionParams ident [] block )
		FunctionParams ident args block -> do
			pairs <- argsToPairs args
			let (types, idents) = ((Prelude.map fst pairs), (Prelude.map snd pairs))
			env2 <- defineFun typename ident args (return (Return NULL))
			env3 <- local (const env2) (insertMultipleDecs idents types)
			ret <- local (const env3) (checkBlock block)
			let retCheck = \x -> case x of
				(Return v) -> (checkTypeVal typename v)
				_ -> False
			if (and (Prelude.map retCheck ret)) then
				return env2
			else throwError "Incompatible types of return expressions"

-- main

runChecker :: Program -> Semantics ()
runChecker program = do
	state <- get
	env   <- ask
	put state
	local (const env) (checkProgram program)

checkProgram :: Program -> Semantics ()
checkProgram (Progr instructions) = do
	(env, ret) <- checkInstructions instructions
	if (Prelude.null ret) then return ()
	else throwError "Return statement on top of the program"
