{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment     (getArgs, lookupEnv)
import           Text.Read              (readMaybe)
import           API.Server             (runServer)

-- | Main: si recibe "--server" o "--server <puerto>", arranca el servidor REST.
--   En cualquier otro caso, muestra un breve mensaje de uso y sale con error.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--server"] ->
      -- Arranca en el puerto de ENV "PORT" o 8080 por defecto
      runOnEnvPort

    ["--server", portStr] ->
      case readMaybe portStr of
        Just p  -> runServer p
        Nothing -> do
          putStrLn "Error: el segundo argumento debe ser un número de puerto válido."
          putStrLn "Uso: basicapi-exe --server [<puerto>]"
          error "Puerto inválido"

    _ -> do
      putStrLn "Uso:"
      putStrLn "  basicapi-exe --server [<puerto>]"
      putStrLn ""
      putStrLn "Ejemplos:"
      putStrLn "  basicapi-exe --server          # arranca en el puerto de ENV PORT o 8080"
      putStrLn "  basicapi-exe --server 3000     # arranca en el puerto 3000"
      error "Argumentos inválidos"

-- | Lee la variable de entorno PORT; si no existe o no es numérico, usa 8080
runOnEnvPort :: IO ()
runOnEnvPort = do
  mPortStr <- lookupEnv "PORT"
  let port = maybe 8080 parseOrDefault mPortStr
  runServer port
  where
    parseOrDefault s =
      case readMaybe s of
        Just p  -> p
        Nothing -> 8080
