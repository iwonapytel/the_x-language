-- automatically generated by BNF Converter
module Main where


import System.IO

import LexGrammar
import ParGrammar
import SkelGrammar
import PrintGrammar
import AbsGrammar

import ErrM
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State
import System.Environment
import Semantic
import StateEnv
import Checker
import Text.Show


main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    (file:_) -> readFile file
    [] -> getContents
  let runInterpreter runType prog = runExceptT (runStateT (runReaderT (runType prog) initialEnv) initialStore)
  case pProgram (myLexer input) of
    (Ok s) -> do
        staticCheckResult <- runInterpreter runChecker s
        case staticCheckResult of
          (Left e) -> putStrLn ("Static checker error: " ++ e)
          (Right e) -> do
            result <- runInterpreter runProgram s
            case result of
              (Left e) -> putStrLn ("Runtime error: " ++ e)
              (Right e) -> putStrLn (show e)
    (Bad s) -> do putStrLn "\nParse Failed...\n"
                  putStrLn s
