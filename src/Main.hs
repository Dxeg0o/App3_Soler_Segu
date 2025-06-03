-- src/Main.hs
{-# OPTIONS_GHC -Wall #-}
module Main where

import System.Environment (getArgs)
import System.Exit       (exitFailure)
import PathFinder        (bestPath)

-- | Función principal: lee argumentos y muestra resultados.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [gridStr, energiaStr] ->
      case (reads gridStr, reads energiaStr) of
        ([(g, "")], [(e, "")]) -> do
          let grid          = (g :: [[Int]])
              energiaInicial = (e :: Int)
          case bestPath grid energiaInicial of
            Nothing -> do
              putStrLn "No hay un camino válido que mantenga energía >= 0."
              exitFailure
            Just (ruta, energiaFinal) -> do
              putStrLn $ "Mejor camino encontrado: " ++ show ruta
              putStrLn $ "Energía final: " ++ show energiaFinal
        _ -> do
          putStrLn "Error: no se pudieron parsear correctamente los argumentos."
          putStrLn "Uso: App3 \"[[2,-3,1,...],[...],...]\" 12"
          exitFailure
    _ -> do
      putStrLn "Cantidad de argumentos inválida."
      putStrLn "Uso: App3 \"<MatrizRuneas>\" <EnergiaInicial>"
      putStrLn "Ejemplo:"
      putStrLn "  App3 \"[[2,-3,1,0,2,3],[-5,4,-2,1,0,-4],[1,3,0,-3,2,2],[2,-1,4,0,-5,1],[0,2,-3,3,4,-1],[1,0,2,-2,1,5]]\" 12"
      exitFailure
