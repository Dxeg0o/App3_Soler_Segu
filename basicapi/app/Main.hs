{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Proxy                (Proxy(..))
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Network.Wai               (Application)
import           Network.Wai.Handler.Warp  (run)
import           Network.Wai.Middleware.Cors
  ( CorsResourcePolicy(..)
  , cors
  , simpleCorsResourcePolicy
  )
import           Servant                   ((:<|>)(..), (:>), JSON, Post, ReqBody, Server, serve)
import           System.Environment        (lookupEnv)

--------------------------------------------------------------------------------
-- 1) Tipos para la petición y la respuesta JSON
--------------------------------------------------------------------------------

-- | Estructura JSON que esperamos en el body del POST
data EchoRequest = EchoRequest
  { message :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON EchoRequest

-- | Estructura JSON que devolvemos
data EchoResponse = EchoResponse
  { echo :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON EchoResponse

--------------------------------------------------------------------------------
-- 2) Definición de la API: POST /echo
--------------------------------------------------------------------------------

-- El endpoint “/echo” recibe un JSON de tipo EchoRequest y devuelve EchoResponse
type API = "echo" :> ReqBody '[JSON] EchoRequest :> Post '[JSON] EchoResponse

-- Implementación del handler
server :: Server API
server (EchoRequest msg) = do
  liftIO $ putStrLn $ "Recibido mensaje: " ++ show msg
  return $ EchoResponse msg

-- Proxy para levantar la API
api :: Proxy API
api = Proxy

--------------------------------------------------------------------------------
-- 3) Aplicación Wai con CORS “*” en POST
--------------------------------------------------------------------------------

-- El middleware cors con simpleCorsResourcePolicy { corsOrigins = Nothing }
-- permite CORS desde cualquier origen. Solo dejamos métodos POST y el header Content-Type.
app :: Application
app = cors (const $ Just policy) $ serve api server
  where
    policy :: CorsResourcePolicy
    policy = simpleCorsResourcePolicy
      { corsOrigins        = Nothing         -- Nothing = “*”
      , corsMethods        = ["POST"]
      , corsRequestHeaders = ["Content-Type"]
      }

--------------------------------------------------------------------------------
-- 4) Main: lee la variable PORT (necesaria en Render) o usa 8080 por defecto
--------------------------------------------------------------------------------

main :: IO ()
main = do
  mPort <- lookupEnv "PORT"
  let port = maybe 8080 read mPort
  putStrLn $ "Arrancando servidor en el puerto " ++ show port
  run port app
