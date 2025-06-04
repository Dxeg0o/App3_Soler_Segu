{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Server (runServer, server, app) where

import Control.Monad.IO.Class (liftIO)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Data.Aeson

import API.Types
import PathFinder (bestPath)

-- | Definición de la API
type PathFinderAPI = 
  "api" :> "findPath" :> ReqBody '[JSON] PathRequest :> Post '[JSON] (Either ErrorResponse PathResponse)
  :<|> "health"  :> Get '[JSON] Value

server :: Server PathFinderAPI
server = findPathHandler :<|> healthHandler

findPathHandler :: PathRequest -> Handler (Either ErrorResponse PathResponse)
findPathHandler (PathRequest grid initialEnergy) = do
  liftIO $ putStrLn $ "Procesando solicitud: Grid size " ++ show (length grid) ++ "x" ++ show (length (head grid)) ++ ", Energy: " ++ show initialEnergy
  case bestPath grid initialEnergy of
    Nothing -> return $ Left $ ErrorResponse "No se encontró un camino válido que mantenga energía >= 0"
    Just (path, finalEnergy) -> do
      liftIO $ putStrLn $ "Camino encontrado: " ++ show path ++ ", Energía final: " ++ show finalEnergy
      return $ Right $ PathResponse path finalEnergy

healthHandler :: Handler Value
healthHandler = return $ object ["status" .= ("OK" :: String), "service" .= ("PathFinder API" :: String)]

-- | Aplicación WAI con middleware CORS que permite cualquier origen
--   Esto inyecta las cabeceras Access-Control-Allow-Origin: *
app :: Application
app = simpleCors $ serve (Proxy :: Proxy PathFinderAPI) server

-- | Función para iniciar el servidor (igual que antes)
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Iniciando servidor PathFinder API en puerto " ++ show port
  putStrLn $ "Endpoints disponibles:"
  putStrLn $ "  POST http://localhost:" ++ show port ++ "/api/findPath"
  putStrLn $ "  GET  http://localhost:" ++ show port ++ "/health"
  run port app
