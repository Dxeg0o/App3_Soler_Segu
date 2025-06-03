{-# OPTIONS_GHC -Wall #-}
module Main where

import System.Environment (getArgs)
import System.Exit       (exitFailure)
import Text.Read         (readMaybe)
import PathFinder        (bestPath)
import Types             (Grid, Energy)
import API.Server        (runServer)

-- | Parsea los argumentos de la línea de comandos
parseArgs :: [String] -> Maybe (Grid, Energy)
parseArgs [gridStr, energyStr] =
  case (readMaybe gridStr, readMaybe energyStr) of
    (Just g, Just e) -> Just (g, e)
    _ -> Nothing
parseArgs _ = Nothing

-- | Muestra el mensaje de uso
showUsage :: IO ()
showUsage = do
  putStrLn "Uso:"
  putStrLn "  Modo CLI: App3 \"<MatrizRuneas>\" <EnergiaInicial>"
  putStrLn "  Modo API: App3 --server [puerto]"
  putStrLn ""
  putStrLn "Ejemplos:"
  putStrLn "  CLI: App3 \"[[2,-3,1,0,2,3],[-5,4,-2,1,0,-4],[1,3,0,-3,2,2],[2,-1,4,0,-5,1],[0,2,-3,3,4,-1],[1,0,2,-2,1,5]]\" 12"
  putStrLn "  API: App3 --server 8080"

-- | Muestra los resultados del pathfinding (modo CLI)
showResults :: Maybe ([(Int, Int)], Int) -> IO ()
showResults Nothing = do
  putStrLn "No hay un camino válido que mantenga energía >= 0."
  exitFailure
showResults (Just (ruta, energiaFinal)) = do
  putStrLn $ "Mejor camino encontrado: " ++ show ruta
  putStrLn $ "Energía final: " ++ show energiaFinal

-- | Función principal: detecta modo CLI o API
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--server"] -> runServer 8080
    ["--server", portStr] -> 
      case readMaybe portStr of
        Just port -> runServer port
        Nothing -> do
          putStrLn "Error: Puerto inválido"
          showUsage
          exitFailure
    _ -> do
      -- Modo CLI tradicional
      case parseArgs args of
        Nothing -> do
          putStrLn "Error: no se pudieron parsear correctamente los argumentos."
          showUsage
          exitFailure
        Just (grid, energiaInicial) -> do
          let result = bestPath grid energiaInicial
          showResults result