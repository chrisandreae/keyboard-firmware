module Main where
import Parser
import TypedAST
import BlockIR
import Indexes
import Output
import Errors

import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Maybe(fromMaybe)
import Data.List
import Control.Monad
import qualified Data.ByteString.Lazy as B

data Opts = Opts {
  outputFile :: FilePath,
  verbose :: Integer
} deriving (Show)

defaultOpts = Opts { outputFile = "a.out", verbose = 0 }

options :: [OptDescr (Opts -> Opts)]
options = [
  (Option ['o'] ["output"] (OptArg (maybe id (\f opts -> opts { outputFile = f })) "FILE") "Output file"),
  (Option ['v'] ["verbose"] (NoArg (\opts -> opts { verbose = (verbose opts) + 1 })) "Verbose")
  ]

getOptions :: IO (Opts, [FilePath])
getOptions = do
  args <- getArgs
  case getOpt Permute options args of
    (o, n, []  ) -> return (foldl (flip ($)) defaultOpts o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo "Usage: keyc [OPTION...] files..." options))

main = do
  (opts, sources) <- getOptions
  when (sources == []) $ ioError (userError $ usageInfo "Usage: keyc [OPTION...] files..." options)
  source   <- fmap concat $ mapM readFile sources

  -- parse, compile, extract IR (for verbose) and output
  result <- return $ do
    ast      <- parseProgram source >>= buildTProgram
    optast   <- optTProgram ast
    ir       <- buildIR optast
    bytecode <- outputProgram ir
    code     <- return $ binaryProgram bytecode
    return (optast, ir, bytecode, code)

  -- handle error
  when (isError result) $ ioError $ userError $ show $ extractError result

  let (optast, ir, bytecode, code) = extractValue result

  when (verbose opts > 1) $ putStrLn "======= AST =======" >> print optast
  when (verbose opts > 0) $ putStrLn "======= IR =======" >> print ir
  when (verbose opts > 2) $ putStrLn "======= Bytecode =======" >> print bytecode

  outFile <- openBinaryFile (outputFile opts) WriteMode
  B.hPut outFile code
  hClose outFile
