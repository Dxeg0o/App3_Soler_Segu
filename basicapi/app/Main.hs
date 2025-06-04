{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Network.Wai               (Application)
import           Network.Wai.Handler.Warp  (run)
import           Network.Wai.Middleware.Cors
  ( CorsResourcePolicy(..)
  , cors
  , simpleCorsResourcePolicy
  )
import           Servant                   ((:<|>)(..), (:>), Capture, Get, JSON, Post, ReqBody, Serve, Server, serve)
import           System.Environment        (lookupEnv)

-- | 1) Definimos un tipo que represente la estructura JSON de la petición
data EchoRequest = EchoRequest
  { message :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON EchoRequest

-- | 2) Definimos un tipo para la respuesta JSON
data EchoResponse = EchoResponse
  { echo :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON EchoResponse

-- | 3) La API tiene un único endpoint POST /echo que recibe 'EchoRequest' y devuelve 'EchoResponse'
type API = "echo" :> ReqBody '[JSON] EchoRequest :> Post '[JSON] EchoResponse

-- | 4) Implementación del handler: simplemente devolvemos el mismo texto que recibimos
server :: Server API
server (EchoRequest msg) = do
  liftIO $ putStrLn $ "Recibido mensaje: " ++ show msg
  return $ EchoResponse msg

-- | 5) Proxy para levantar la API
api :: Proxy API
api = Proxy

-- | 6) Añadimos CORS para permitir cualquier origen en métodos POST
--    - corsOrigins = Nothing indica “*”
--    - corsMethods = ["POST"] permite solo POST (puedes agregar GET, PUT, etc., si los necesitas)
--    - corsRequestHeaders default ya incluye Content-Type; si necesitas más, añádelos
app :: Application
app = cors (const $ Just policy) $ serve api server
  where
    policy :: CorsResourcePolicy
    policy = simpleCorsResourcePolicy
      { corsOrigins        = Nothing
      , corsMethods        = ["POST"]
      , corsRequestHeaders = ["Content-Type"]
      }

-- | 7) Obtenemos el puerto desde la variable de entorno PORT (necesario en Render)
main :: IO ()
main = do
  mPort <- lookupEnv "PORT"
  let port = maybe 8080 read mPort
  putStrLn $ "Arrancando servidor en el puerto " ++ show port
  run port app
