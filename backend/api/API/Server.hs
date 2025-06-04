{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Server (runServer, server, app) where

import Control.Monad.IO.Class (liftIO)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Data.Aeson
import Control.Exception (SomeException, try) -- Add this import
import Data.List (headMay) -- Add this import

import API.Types
import PathFinder (bestPath)

-- | Definición de la API
type PathFinderAPI = 
  "api" :> "findPath" :> ReqBody '[JSON] PathRequest :> Post '[JSON] (Either ErrorResponse PathResponse)
  :<|> "health" :> Get '[JSON] Value

-- | Implementación de los endpoints
server :: Server PathFinderAPI
server = findPathHandler :<|> healthHandler

-- | Handler para encontrar el mejor camino (FIXED)
findPathHandler :: PathRequest -> Handler (Either ErrorResponse PathResponse)
findPathHandler (PathRequest grid initialEnergy) = do
  let rows = length grid
  let cols = if null grid then 0 else length (head grid)
  liftIO $ putStrLn $ "Procesando solicitud: Grid size " ++ show rows ++ "x" ++ show cols ++ ", Energy: " ++ show initialEnergy

  if null grid
    then return $ Left $ ErrorResponse "Grid cannot be empty"
    else do
      result <- liftIO $ try (evaluate $ bestPath grid initialEnergy) :: Handler (Either SomeException (Maybe (Path, Energy)))
      case result of
        Left ex -> do
          liftIO $ putStrLn $ "Error en bestPath: " ++ show ex
          return $ Left $ ErrorResponse "Error interno al procesar el grid"
        Right Nothing -> return $ Left $ ErrorResponse "No se encontró un camino válido que mantenga energía >= 0"
        Right (Just (path, finalEnergy)) -> do
          liftIO $ putStrLn $ "Camino encontrado: " ++ show path ++ ", Energía final: " ++ show finalEnergy
          return $ Right $ PathResponse path finalEnergy

-- | Handler para verificar que el servidor está funcionando
healthHandler :: Handler Value
healthHandler = return $ object ["status" .= ("OK" :: String), "service" .= ("PathFinder API" :: String)]

-- | Configuración de CORS (unchanged)
corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
  { corsRequestHeaders = ["Content-Type"]
  , corsMethods = ["GET", "POST", "OPTIONS"]
  , corsOrigins = Just (["https://app3-soler-segu.vercel.app", "http://localhost:3000"], True)
  , corsRequireOrigin = False
  }

-- | Aplicación WAI con middleware CORS (unchanged)
app :: Application
app = cors (const $ Just corsPolicy) $ serve (Proxy :: Proxy PathFinderAPI) server

-- | Función para iniciar el servidor (unchanged)
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Iniciando servidor PathFinder API en puerto " ++ show port
  putStrLn $ "Endpoints disponibles:"
  putStrLn $ "  POST http://localhost:" ++ show port ++ "/api/findPath"
  putStrLn $ "  GET  http://localhost:" ++ show port ++ "/health"
  run port app