{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Server (runServer, server, app) where

import Control.Monad.IO.Class  (liftIO)
import Data.Aeson (Value, FromJSON, ToJSON, object, (.=))
import GHC.Generics            (Generic)
import Network.Wai             (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy(..)
  , simpleCorsResourcePolicy
  , cors
  )
import Servant

import API.Types               (ErrorResponse(..), PathResponse(..))
import PathFinder              (bestPath)

--------------------------------------------------------------------------------
-- 1) Define aquí el tipo PathRequest con sus instancias JSON
--------------------------------------------------------------------------------

data PathRequest = PathRequest
  { prGrid          :: [[Int]]
  , prInitialEnergy :: Int
  } deriving (Show, Generic)

instance FromJSON PathRequest
instance ToJSON   PathRequest

--------------------------------------------------------------------------------
-- 2) Definición de la API (igual que tenías)
--------------------------------------------------------------------------------

type PathFinderAPI =
       "api" :> "findPath" :> ReqBody '[JSON] PathRequest :> Post '[JSON] (Either ErrorResponse PathResponse)
  :<|> "health"  :> Get '[JSON] Value

--------------------------------------------------------------------------------
-- 3) Implementación de los handlers
--------------------------------------------------------------------------------

-- Handler que calcula el mejor camino o devuelve un error
findPathHandler :: PathRequest -> Handler (Either ErrorResponse PathResponse)
findPathHandler (PathRequest grid initialEnergy) = do
  liftIO $ putStrLn
    $  "Procesando solicitud: Grid size "
    ++ show (length grid)
    ++ "x"
    ++ show (length (head grid))
    ++ ", Energy: "
    ++ show initialEnergy

  case bestPath grid initialEnergy of
    Nothing ->
      return $ Left $ ErrorResponse "No se encontró un camino válido que mantenga energía >= 0"
    Just (path, finalEnergy) -> do
      liftIO $ putStrLn
        $  "Camino encontrado: "
        ++ show path
        ++ ", Energía final: "
        ++ show finalEnergy
      return $ Right $ PathResponse path finalEnergy

-- Handler para chequear que el servidor está arriba
healthHandler :: Handler Value
healthHandler = return $ object
  [ "status"  .= ("OK" :: String)
  , "service" .= ("PathFinder API" :: String)
  ]

-- Conecta los handlers con la especificación de la API
server :: Server PathFinderAPI
server = findPathHandler :<|> healthHandler

--------------------------------------------------------------------------------
-- 4) Política CORS que permita cualquier origen (incluye OPTIONS)
--------------------------------------------------------------------------------

corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
  { corsRequestHeaders = ["Content-Type", "Authorization", "Accept"]
  , corsMethods        = ["GET", "POST", "OPTIONS"]
  , corsOrigins        = Just ["*"]
  , corsRequireOrigin  = True
  , corsMaxAge         = Just 86400
  }

--------------------------------------------------------------------------------
-- 5) Aplicación WAI con CORS aplicado antes de `serve`
--------------------------------------------------------------------------------

app :: Application
app = cors (const $ Just corsPolicy) $ serve (Proxy :: Proxy PathFinderAPI) server

--------------------------------------------------------------------------------
-- 6) Función main para levantar el servidor
--------------------------------------------------------------------------------

runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Iniciando servidor PathFinder API en puerto " ++ show port
  putStrLn $ "Endpoints disponibles:"
  putStrLn $ "  POST http://localhost:" ++ show port ++ "/api/findPath"
  putStrLn $ "  GET  http://localhost:" ++ show port ++ "/health"
  run port app
