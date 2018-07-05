module Semantic where

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


-- todo tutaj zrób jumpy
semInstructions :: [Instruction] -> Semantics (Env, Ret)
semInstructions (x:xs) = do
  traceShowM x
  (env, ret) <- semInstruction x
  case ret of
    None -> local (const env) (semInstructions xs)
    _ -> return (env, ret)

semInstructions [] = do
  env <- ask
  return (env, None)

semInstruction :: Instruction -> Semantics (Env, Ret)
semInstruction i = do
  env <- ask
  case i of
    StmInstruction s -> do
      ret <- semStatement s
      return (env, ret)
    DecInstruction d -> do
      env2 <- semDeclaration d
      return (env2, None)
    ExpInstruction e -> do
      val <- semExp e
      traceShowM val
      return (env, None)
    BlockInstruction b -> do
      case b of
        BBlock i -> do
          (env2, ret) <- semInstructions i
          return (env, ret)
        EmptyBlock -> return (env, None)


-- Statements semantic
semStatement :: Stm -> Semantics Ret
semStatement stm = case stm of
    EmptyStm   -> return None
    PrintStm x -> semPrintStm x
    SelectStm x -> case x of
      IfStm e i       -> semIfElseStm e i (StmInstruction EmptyStm)
      IfElseStm e i1 i2 -> semIfElseStm e i1 i2

semPrintStm :: Exp -> Semantics Ret
semPrintStm expr = do
    eval <- semExp expr
    printVal eval
    return None

printVal :: Val -> Semantics ()
printVal val = case val of
  (INT i)  -> liftIO (print i)
  (BOOL i) -> liftIO (print i)

semIfElseStm :: Exp -> Instruction -> Instruction -> Semantics Ret
semIfElseStm e i1 i2 = do
  val <- semExp e
  cond <- valToCondition val
  (_, ret) <- if cond then semInstruction i1
              else semInstruction i2
  return ret

semWhileStm :: Exp -> Instruction -> Semantics Ret
semWhileStm e i = do
  val <- semExp e
  cond <- valToCondition val
  if not cond then return None
  else do
    (_, ret) <- semInstruction i
    case ret of
      Break ->  return None
      Return v ->  return ret
      _ ->  do
        ret2 <- (semWhileStm e i)
        return ret2

valToCondition :: Val -> Semantics Bool
valToCondition val = do
  case val of
   (BOOL b) -> return b
   (INT 0)  -> return False
   (INT _)  -> return True
   _        -> throwError "Unsupported condition type"

-- Declarations semantics
semDeclaration :: Dec -> Semantics Env
semDeclaration dec = do
  case dec of
    VarDec typename item -> semVarDec typename item

semVarDec :: TypeName ->  Item -> Semantics Env
semVarDec typename item = do
  case item of
    UninitedVar ident    -> varDecl ident (defaultTypeValue typename)
    InitedVar ident expr -> do
      val <- semExp expr
      if not (checkTypeVal typename val) then
        throwError "Incompatible value type for given type identifier"
      else
        varDecl ident val
    UninitedArr ident arrsize -> case arrsize of
      ValArrSize det -> do
        detVal <- semExp det
        case detVal of
          (INT i) -> do
            let defaultVals = (replicate i (INT 0))
            putArray ident detVal defaultVals
          _ -> throwError "Incompatible array size"
      EmptyArrSize -> do
        varDecl ident (ARR 0 Map.empty)
    InitedArr ident arrsize (Earray expr) -> do
      vals <- semMultipleVals expr
      _ <- checkArrVals typename vals
      case arrsize of
        ValArrSize det -> do
          detVal <- semExp det
          putArray ident detVal vals
        EmptyArrSize ->
          putArray ident (INT $ length vals) vals



-- replicate problem
--

{-
semFunDec :: Dec -> Semantics Env
-}

-- main
runProgram :: Program -> Semantics Env
runProgram program = do
  state <- get
  env   <- ask
  put state
  local (const env) (semProgram program)

semProgram :: Program -> Semantics Env
semProgram (Progr instructions) = do
  (env, ret) <- semInstructions instructions
  return env
