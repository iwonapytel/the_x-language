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
    (Elor exp1 exp2)  -> do
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
  EVar i -> setVarVal i val
  _       -> throwError "Unsupported yet"

semAssignmentOp_ :: (Int -> Int -> Int) -> Exp -> Val -> Semantics Val
semAssignmentOp_ op var val = do
  oldValue <- semExp var
  case (oldValue, val) of
    (INT i, INT j) -> do
      semAssignment_ var (INT (op i j))
    _       -> throwError "Unsupported assignment type"

defaultTypeValue :: TypeName -> Val
defaultTypeValue typename = case typename of
  TInt  -> (INT 0)
  TBool -> (BOOL False)

semMultipleVals :: [Exp]-> Semantics [Val]
semMultipleVals (e:expr) = do
   val <- semExp e
   vals <- semMultipleVals expr
   return ([val] ++ vals)

semMultipleVals [] = return []


-- StaticChecker
checkTypePair :: Val -> Val -> Semantics ()
checkTypePair val1 val2 = do
  case (val1, val2) of
    (INT _, INT _) -> return ()
    (BOOL _, BOOL _) -> return ()
    (ARR s1 mmap1, ARR s2 mmap2) -> do
      if (s1 == 0) || (s2 == 0) then return ()
      else do
        let (Just loc1) = Map.lookup 0 mmap1
        let (Just loc2) = Map.lookup 0 mmap2
        val1 <- getLocVal loc1
        val2 <- getLocVal loc2
        checkTypePair val1 val2
    _ -> throwError $ "Incompatible types: " ++ show val1 ++ show val2


checkTypeVal :: TypeName -> Val -> Bool
checkTypeVal typename val = case (typename, val) of
  (TInt, INT _) -> True
  (TBool, BOOL _) -> True
  (_, _) -> False

checkValType :: Val -> TypeName -> Bool
checkValType val typename = case (val, typename) of
  (INT _, TInt) -> True
  (BOOL _, TBool) -> True
  (_, _) -> False

checkArrVals :: TypeName -> [Val] -> Semantics ()
checkArrVals typename (val:vals) = do
  if (checkTypeVal typename val) then (checkArrVals typename vals)
  else throwError "Incompatible types of array initialization"

checkArrVals typename [] = return ()
