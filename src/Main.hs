{-# LANGUAGE DoAndIfThenElse #-}
import Criterion.Main
import System.IO.Silently (silence)

import System.Environment
import System.Exit
import System.IO
import System.Timeout
import Control.Monad
import Data.List
import Jana.Parser
import Jana.Eval (runProgram)
import Jana.Types (defaultOptions, EvalOptions(..))
import Jana.Invert


data Options = Options
  { timeOut :: Int
  , invert :: Bool
  , evalOpts :: EvalOptions
  , benchmark :: Bool
  , output :: String
  }

defaults = Options
  { timeOut = -1
  , invert = False
  , evalOpts = defaultOptions
  , benchmark = False
  , output = "report.html"
  }

usage = "usage: jana [options] <file>\n\
        \options:\n\
        \  -m           use 32-bit modular arithmetic\n\
        \  -tN          timeout after N seconds\n\
        \  -i           print inverted program\n\
        \  -b           run benchmarking\n\
        \  -o           output file"

parseArgs :: IO (Maybe ([String], Options))
parseArgs =
  do args <- getArgs
     (flags, files) <- return $ splitArgs args
     case checkFlags flags of
       Left err   -> putStrLn err >> return Nothing
       Right opts -> return $ Just (files, opts)

splitArgs :: [String] -> ([String], [String])
splitArgs = partition (\arg -> head arg == '-' && length arg > 1)

checkFlags :: [String] -> Either String Options
checkFlags = foldM addOption defaults

addOption :: Options -> String -> Either String Options
addOption opts@(Options { evalOpts = evalOptions }) "-m" =
  return $ opts { evalOpts = evalOptions { modInt = True } }
addOption opts ('-':'t':time) =
  case reads time of
    [(timeVal, "")] -> return $ opts { timeOut = timeVal }
    _               -> Left "non-number given to -t option"
addOption opts ('-':'o':output) = return opts { output = output }
addOption opts "-i" = return opts { invert = True }
addOption opts "-b" = return opts { benchmark = True }
addOption _ f = Left $ "invalid option: " ++ f

loadFile :: String -> IO String
loadFile "-"      = getContents
loadFile filename = readFile filename

printInverted :: String -> IO ()
printInverted filename =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> print $ invertProgram prog

parseAndRun :: String -> EvalOptions -> IO ()
parseAndRun filename evalOptions =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> runProgram filename prog evalOptions

main :: IO ()
main = do args <- parseArgs
          case args of
            Just ([file], opts) -> do
              when (invert opts) $ printInverted file
              text <- loadFile file
              case parseProgram file text of
                Left err   -> print err >> (exitWith $ ExitFailure 1)
                Right prog ->
                  if benchmark opts then
                    withArgs ["-o", output opts] $
                      defaultMain [bgroup file [bench "1" $ whnfIO (go file prog opts)]]
                  else
                  go file prog opts
            _ -> putStrLn usage
          where go file prog opts = do
                  res <- timeout (timeOut opts * 1000000) (runProgram file prog (evalOpts opts))
                  print res
                  case res of
                    Nothing -> exitWith $ ExitFailure 124
                    Just r  -> return ()
