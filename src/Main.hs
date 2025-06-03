{-# OPTIONS_GHC -Wall #-}
module Main where

import System.Environment (getArgs)
import System.Exit       (exitFailure)
import PathFinder        (bestPath)
import Types             (Grid, Energy)

-- | Parsea los argumentos de la línea de comandos
parseArgs :: [String] -> Maybe (Grid, Energy)
parseArgs [gridStr, energyStr] =
  case (reads gridStr, reads energyStr) of
    ([(g, "")], [(e, "")]) -> Just (g, e)
    _ -> Nothing
parseArgs _ = Nothing

-- | Muestra el mensaje de uso
showUsage :: IO ()
showUsage = do
  putStrLn "Cantidad de argumentos inválida."
  putStrLn "Uso: App3 \"<MatrizRuneas>\" <EnergiaInicial>"
  putStrLn "Ejemplo:"
  putStrLn "  App3 \"[[2,-3,1,0,2,3],[-5,4,-2,1,0,-4],[1,3,0,-3,2,2],[2,-1,4,0,-5,1],[0,2,-3,3,4,-1],[1,0,2,-2,1,5]]\" 12"

-- | Muestra los resultados del pathfinding
showResults :: Maybe ([(Int, Int)], Int) -> IO ()
showResults Nothing = do
  putStrLn "No hay un camino válido que mantenga energía >= 0."
  exitFailure
showResults (Just (ruta, energiaFinal)) = do
  putStrLn $ "Mejor camino encontrado: " ++ show ruta
  putStrLn $ "Energía final: " ++ show energiaFinal

-- | Función principal: lee argumentos y muestra resultados.
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Nothing -> do
      putStrLn "Error: no se pudieron parsear correctamente los argumentos."
      putStrLn "Uso: App3 \"[[2,-3,1,...],[...],...]\" 12"
      exitFailure
    Just (grid, energiaInicial) -> do
      let result = bestPath grid energiaInicial
      showResults result