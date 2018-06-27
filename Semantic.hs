module Semantic where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Text.Show
import Debug.Trace
import Data.Map as Map
import ErrM
import AbsGrammar
import LexGrammar
import ParGrammar
import PrintGrammar

type Result = ExceptT String IO
type Semantics = ReaderT Env (StateT Store (Result))

type Var = Ident
type FuncId = Ident
type Loc = Int
data Val = INT Int | BOOL Bool deriving (Eq, Show, Ord)

type Store = Map Loc Val
type Env = Map Var Loc

-- Initial states
emptyEnv :: Env
emptyEnv = Map.empty

initialStore :: Store
initialStore = Map.singleton (0) (INT 1)

findLoc :: Ident -> Semantics Loc
findLoc ident = do
  env <- ask
  let loc = Map.lookup ident env
  case loc of
    Just loc -> return loc
    Nothing -> throwError "Undeclared var"

setLocVal :: Loc -> Val -> Semantics ()
setLocVal loc val = do
  modify (Map.insert loc val)

setVarVal :: Ident -> Val -> Semantics ()
setVarVal ident val = do
  loc <- findLoc ident
  setLocVal loc val

getLocVal :: Loc -> Semantics Val
getLocVal loc = do--
  Just val <- gets (Map.lookup loc)
  return val

getVarVal :: Ident -> Semantics Val
getVarVal ident = do
  loc <- findLoc ident
  val <- getLocVal loc
  return val

varDecl :: Ident -> (Maybe Val) -> Semantics Env
varDecl ident val = do
  env <- ask
  if (Map.member ident env) then
    throwError "Muplitple declarations of same ident"
  else do
    loc <- case val of
      Just val -> newLoc val
      Nothing -> newLoc (INT 0)
    return (Map.insert ident loc env)

newLoc :: Val -> Semantics Loc
newLoc val = do
  Just (INT loc) <- gets (Map.lookup 0)
  modify (Map.insert loc val)
  modify (Map.insert 0 (INT (loc + 1)))
  return loc

--- Expressions semantics

semBinExp :: Exp -> Exp -> Semantics (Val, Val)
semBinExp e1 e2 = do
  _e1 <- semExp e1
  _e2 <- semExp e2
  return(_e1, _e2)

semBinIntExp :: Exp -> Exp -> Semantics (Int, Int)
semBinIntExp e1 e2 = do
  (_e1, _e2) <- semBinExp e1 e2
  case (_e1, _e2) of
    (INT a, INT b) -> return (a, b)
    _ -> throwError "Incompatible types for arithmetic expression"

semBinBoolExp :: Exp -> Exp -> Semantics (Bool, Bool)
semBinBoolExp b1 b2 = do
  (_b1, _b2) <- semBinExp b1 b2
  case (_b1, _b2) of
    (BOOL a, BOOL b) -> return (a, b)
    _ -> throwError "Incompatible types for boolean expression"




semExp :: Exp -> Semantics Val
semExp ex = do
  case ex of
    (ELitInt i)         -> return (INT (fromInteger i))
    (ELitBool b)        -> case b of
                            ETrue ->  return (BOOL True)
                            EFalse -> return (BOOL False)
    (EVar ident)        -> getVarVal ident
    (Elor exp1 exp2)    -> do
           (e1, e2) <- semBinBoolExp exp1 exp2
           return $ BOOL (e1 || e2)
    (Eland exp1 exp2) -> do
          (a, b) <- semBinBoolExp exp1 exp2
          return $ BOOL (a && b)
    (Eeq exp1 exp2) -> do
           (a, b) <- semBinExp exp1 exp2
           return $ BOOL $ a == b
    (Eneq exp1 exp2) -> do
           BOOL a <- semExp (Eeq exp1 exp2)
           return $ BOOL $ not a
    (Elthen exp1 exp2) -> do
           (a, b) <- semBinExp exp1 exp2
           return $ BOOL $ a < b
    (Egrthen exp1 exp2) -> do
          (a, b) <- semBinExp exp1 exp2
          return $ BOOL $ a > b
    (Ele exp1 exp2) -> do
          (a, b) <- semBinExp exp1 exp2
          return $ BOOL $ a <= b
    (Ege exp1 exp2) -> do
          (a, b) <- semBinExp exp1 exp2
          return $ BOOL $ a >= b
    (Eplus exp1 exp2) -> do
          pair <- semBinExp exp1 exp2
          case pair of
            (INT a, INT b) -> return $ INT $ a + b
            --(STRING a, STRING b) -> return $ STRING $ a ++ b
            _ -> throwError "Cannot use + operator on given types"
    (Eminus exp1 exp2) -> do
          (a, b) <- semBinIntExp exp1 exp2
          return $ INT $ a - b
    (Etimes exp1 exp2) -> do
          (a, b) <- semBinIntExp exp1 exp2
          return $ INT $ a * b
    (Ediv exp1 exp2) -> do
          (a, b) <- semBinIntExp exp1 exp2
          if (b == 0) then throwError "Division by zero"
          else return $ INT $ div a b
    --(Epreop unary_operator exp) -> do
    --      a <- transExp exp
    --      transUnary_operator unary_operator a
    _      -> throwError "Unsupported expression type"

-- todo tutaj zrób jumpy
semInstructions :: [Instruction] -> Semantics Env
semInstructions (x:xs) = do
  traceShowM x
  env <- semInstruction x
  local (const env) (semInstructions xs)

semInstructions [] = do
  env <- ask
  return env

semInstruction :: Instruction -> Semantics Env
semInstruction i = do
  env <- ask
  case i of
    StmInstruction s -> do
      semStatement s
      return env
    DecInstruction d -> do
      env2 <- semDeclaration d
      return env2
    ExpInstruction e -> do
      val <- semExp e
      traceShowM val
      return env

-- Statements semantic
semStatement :: Stm -> Semantics ()
semStatement stm =
  return ()
{-
semEmptyStm :: Stm -> Semantics ()
semBlockStm :: Stm -> Semantics ()
semExpStm :: Stm -> Semantics ()
semPrintStm :: Stm -> Semantics ()
semSelectStm :: Stm -> Semantics ()
-}
-- Declarations semantics
semDeclaration :: Dec -> Semantics Env
semDeclaration dec = do
  env <- ask
  return env
{-
semVarDec :: Dec -> Semantics Env
semFunDec :: Dec -> Semantics Env
-}
runProgram :: Program -> Semantics Env
runProgram program = do
  state <- get
  env   <- ask
  put state
  local (const env) (semProgram program)

semProgram :: Program -> Semantics Env
semProgram (Progr instructions) = do
  env <- semInstructions instructions
  return env
