module Main where

import System.Environment (getArgs)
import System.Exit       (exitFailure)
import Text.Read         (readMaybe)
import PathFinder        (bestPath)
import Types             (Grid, Energy)
import API.Server        (runServer)

-- (...) parseArgs, showUsage, showResults (...)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--server"] -> runServer 8080
    ["--server", portStr] ->
      case readMaybe portStr of
        Just port -> runServer port
        Nothing   -> do
          putStrLn "Error: Puerto inválido"
          showUsage
          exitFailure
    _ -> do
      -- Modo CLI: uso de bestPath directamente
      case parseArgs args of
        Nothing -> do
          putStrLn "Error: no se pudieron parsear correctamente los argumentos."
          showUsage
          exitFailure
        Just (grid, energiaInicial) -> do
          let result = bestPath grid energiaInicial
          showResults result
