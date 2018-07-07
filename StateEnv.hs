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
type Fun = [Val] -> Semantics Val
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
setLocVal loc newVal = do
	oldVal <- getLocVal loc
	_ <- checkTypePair oldVal newVal
	modify (Map.insert loc newVal)

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
	loc <- newLoc val
	return Env { venv = Map.insert ident loc (venv env), fenv = (fenv env) }

newLoc :: Val -> Semantics Loc
newLoc val = do
	Just (INT loc) <- gets (Map.lookup 0)
	modify (Map.insert loc val)
	modify (Map.insert 0 (INT (loc + 1)))
	return loc

getArrElemLoc :: Ident -> Val -> Semantics Loc
getArrElemLoc ident index = do
	arr <- getVarVal ident
	case (index, arr) of
		(INT i, ARR s m) ->
			if (i >= s) then throwError "Array index out of range"
			else do
				let (Just loc) = Map.lookup i m
				return loc
		_ -> throwError "Getting element of array: wrong types"

getArrElem :: Ident -> Val -> Semantics Val
getArrElem ident index = do
	loc <- getArrElemLoc ident index
	getLocVal loc

setArrElem :: Ident -> Val -> Val -> Semantics Val
setArrElem ident index val = do
	loc <- getArrElemLoc ident index
	_ <- setLocVal loc val
	return val

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

insertMultipleDecs :: [Ident] -> [Val] -> Semantics Env
insertMultipleDecs (i:idents) (v:vals) = do
	env <- varDecl i v
	local (const env) (insertMultipleDecs idents vals)

insertMultipleDecs [] [] = do
	env <- ask
	return env

insertMultipleDecs _ _ = throwError "Wrong amount of idents and values"

-- Functions env
defineFun :: TypeName -> Ident -> [Arg] -> Semantics Ret -> Semantics Env
defineFun typename ident args instructions = do
	env <- ask
	if (Map.member ident (fenv env)) then
		throwError ("Multiple declarations of a function with a same ident" ++ (show ident))
	else do
		pairs <- argsToPairs args
		let (types, idents) = ((Prelude.map fst pairs), (Prelude.map snd pairs))
		let env3 = Env {venv = (venv env), fenv = Map.insert ident funcBody (fenv env)}
			where
				funcBody = \params -> do
						checkFuncArgs types params
						env2 <- local (const env3) (insertMultipleDecs idents params)
						traceShowM env2
						ret <- local (const env2) instructions
						case ret of
								Return val ->
									if (checkTypeVal typename val) then return val
									else throwError "Incompatible return type"
								_ -> throwError "Function needs to return some value"
		return env3


argsToPairs :: [Arg] -> Semantics [(Val, Ident)]
argsToPairs (a:args) = do
	case a of
		SimpleArg typename ident -> do
			let pair = [((defaultTypeValue typename), ident)]
			pairsList <- argsToPairs args
			return (pair ++ pairsList)
		ArrArg typename ident -> do
			loc <- newLoc (defaultTypeValue typename)
			let defaultArrValue = (ARR 1 (Map.singleton 0 loc))
			pairsList <- argsToPairs args
			return ([(defaultArrValue, ident)] ++ pairsList)

argsToPairs [] = return []

callFunction :: Ident -> [Val] -> Semantics Val
callFunction ident params = do
	env <- ask
	let func = Map.lookup ident (fenv env)
	case func of
		Just func -> (func params)
		Nothing -> throwError "Undeclared function"

-- Types checking
checkTypePair :: Val -> Val -> Semantics ()
checkTypePair val1 val2 = do
	case (val1, val2) of
		(INT _, INT _) -> return ()
		(BOOL _, BOOL _) -> return ()
		(ARR s1 mmap1, ARR s2 mmap2) -> do
			let (Just loc1) = Map.lookup 0 mmap1
			let (Just loc2) = Map.lookup 0 mmap2
			val1 <- getLocVal loc1
			val2 <- getLocVal loc2
			checkTypePair val1 val2
		(NULL, _) -> return ()
		(_, NULL) -> return ()
		_ -> throwError $ "Incompatible types: " ++ show val1 ++ show val2


checkTypeVal :: TypeName -> Val -> Bool
checkTypeVal typename val = case (typename, val) of
	(TInt, INT _) -> True
	(TBool, BOOL _) -> True
	(_, _) -> False


checkArrVals :: TypeName -> [Val] -> Semantics ()
checkArrVals typename (val:vals) = do
	if (checkTypeVal typename val) then (checkArrVals typename vals)
	else throwError "Incompatible types of array initialization"

checkArrVals typename [] = return ()

checkFunctionRetValue :: TypeName -> Ret -> Semantics ()
checkFunctionRetValue typename ret = do
	case ret of
		(Return val) -> if not (checkTypeVal typename val) then
											throwError "Wrong return type of declared function"
										else
											return ()
		_ -> throwError "Wrong return statement of function"

checkFuncArgs :: [Val] -> [Val] -> Semantics ()
checkFuncArgs (t:types) (v:values) = do
	checkTypePair t v
	checkFuncArgs types values

checkFuncArgs [] [] = return ()
checkFuncArgs _ _ = throwError "Wrong amount of parameters"

defaultTypeValue :: TypeName -> Val
defaultTypeValue typename = case typename of
	TInt  -> (INT 0)
	TBool -> (BOOL False)
