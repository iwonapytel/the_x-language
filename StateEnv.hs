module StateEnv where

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

type Result = ExceptT String IO
type Semantics = ReaderT Env (StateT Store (Result))

type Var = Ident
type FuncId = Ident
type Fun = [Val] -> Semantics Ret
type Loc = Int
data Val = INT Int | BOOL Bool | ARR Int (Map Int Loc) | STRUCT (Map Ident Loc) | NULL deriving (Eq, Show, Ord)
data Ret = Return Val | Break | Continue | None
data Env = Env {venv :: Venv,
                fenv :: Fenv}

type Store = Map Loc Val
type Venv = Map Var Loc
type Fenv = Map Var Fun

instance Show Env where
  show (Env v f) = show (v) ++ show (keys f)


-- Initial states
initialEnv :: Env
initialEnv = Env { venv = Map.empty, fenv = Map.empty}

initialStore :: Store
initialStore = Map.singleton (0) (INT 1)

-- Utils
findLoc :: Ident -> Semantics Loc
findLoc ident = do
  env <- ask
  let loc = Map.lookup ident (venv env)
  case loc of
    Just loc -> return loc
    Nothing -> throwError "Undeclared var"

setLocVal :: Loc -> Val -> Semantics ()
setLocVal loc val = do
  modify (Map.insert loc val)

setVarVal :: Ident -> Val -> Semantics Val
setVarVal ident val = do
  loc <- findLoc ident
  setLocVal loc val
  return val

getLocVal :: Loc -> Semantics Val
getLocVal loc = do--
  Just val <- gets (Map.lookup loc)
  return val

getVarVal :: Ident -> Semantics Val
getVarVal ident = do
  loc <- findLoc ident
  val <- getLocVal loc
  return val

varDecl :: Ident -> Val -> Semantics Env
varDecl ident val = do
  env <- ask
  if (Map.member ident (venv env)) then
    throwError "Multiple declarations of same ident"
  else do
    loc <- newLoc val
    return Env { venv = Map.insert ident loc (venv env), fenv = (fenv env) }

newLoc :: Val -> Semantics Loc
newLoc val = do
  Just (INT loc) <- gets (Map.lookup 0)
  modify (Map.insert loc val)
  modify (Map.insert 0 (INT (loc + 1)))
  return loc

getArrElem :: Ident -> Val -> Semantics Val
getArrElem ident val = do
   arr <- getVarVal ident
   case (val, arr) of
     (INT i, ARR s m) ->
        if (i >= s) then throwError "Array index out of range"
        else do
          let (Just loc) = Map.lookup i m
          arrval <- getLocVal loc
          return arrval
     _ -> throwError "Getting element of array: wrong types"

putArray :: Ident -> Val -> [Val] -> Semantics Env
putArray ident det vals = do
  case det of
    (INT arrsize) -> do
      if (not (arrsize == (length vals)) || arrsize < 0) then
        throwError "Wrong size of array initializers list"
      else do
        locs <- insertMultipleVals vals
        varDecl ident (ARR arrsize (fromList (zip [0..(arrsize-1)] locs)))
    _ -> throwError "Wrong array size type"

insertMultipleVals :: [Val] -> Semantics [Loc]
insertMultipleVals (v:vals) = do
  loc <- newLoc v
  locsList <- insertMultipleVals vals
  return ([loc] ++ locsList)

insertMultipleVals [] = return []
