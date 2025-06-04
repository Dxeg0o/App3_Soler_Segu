{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Server (runServer, server, app) where

import Control.Monad.IO.Class (liftIO)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
  ( cors
  , simpleCorsResourcePolicy
  , simpleHeaders
  )
import Servant
import Data.Aeson

import API.Types
import PathFinder (bestPath)

-- | Definición de la API
type PathFinderAPI = 
  "api" :> "findPath" :> ReqBody '[JSON] PathRequest :> Post '[JSON] (Either ErrorResponse PathResponse)
  :<|> "health" :> Get '[JSON] Value

-- | Implementación de los endpoints
server :: Server PathFinderAPI
server = findPathHandler :<|> healthHandler

-- | Handler para encontrar el mejor camino
findPathHandler :: PathRequest -> Handler (Either ErrorResponse PathResponse)
findPathHandler (PathRequest grid initialEnergy) = do
  liftIO $ putStrLn $ "Procesando solicitud: Grid size " ++ show (length grid) ++ "x" ++ show (length $ head grid) ++ ", Energy: " ++ show initialEnergy
  
  case bestPath grid initialEnergy of
    Nothing -> return $ Left $ ErrorResponse "No se encontró un camino válido que mantenga energía >= 0"
    Just (path, finalEnergy) -> do
      liftIO $ putStrLn $ "Camino encontrado: " ++ show path ++ ", Energía final: " ++ show finalEnergy
      return $ Right $ PathResponse path finalEnergy

-- | Handler para verificar que el servidor está funcionando
healthHandler :: Handler Value
healthHandler = return $ object ["status" .= ("OK" :: String), "service" .= ("PathFinder API" :: String)]

-- | Aplicación WAI con middleware CORS
app :: Application
app = cors (const $ Just policy) $ serve (Proxy :: Proxy PathFinderAPI) server
  where
    policy = simpleCorsResourcePolicy
      { corsOrigins = Just (["https://app3-soler-segu.vercel.app"], True)
      , corsRequestHeaders = "Content-Type" : simpleHeaders
      }

-- | Función para iniciar el servidor
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Iniciando servidor PathFinder API en puerto " ++ show port
  putStrLn $ "Endpoints disponibles:"
  putStrLn $ "  POST http://localhost:" ++ show port ++ "/api/findPath"
  putStrLn $ "  GET  http://localhost:" ++ show port ++ "/health"
  run port app