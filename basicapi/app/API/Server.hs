{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Server (runServer) where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Proxy                (Proxy(..))
import           GHC.Generics              (Generic)
import           Network.Wai               (Application)
import           Network.Wai.Handler.Warp  (run)
import           Network.Wai.Middleware.Cors
  ( CorsResourcePolicy(..)
  , cors
  , simpleCorsResourcePolicy
  )
import           Servant                   (Handler, Server, err400, throwError, (:<|>)(..)
                                           , (:>), JSON, Post, ReqBody, serve)
import qualified Servant
import           Types                     (Grid, Energy, Pos)        -- de src/Types.hs
import           PathFinder                (bestPath)                  -- de src/PathFinder.hs

--------------------------------------------------------------------------------
-- 1) Tipos de petición y respuesta en JSON
--------------------------------------------------------------------------------

-- | JSON de entrada para /findPath
data PathRequest = PathRequest
  { prGrid         :: Grid    -- la matriz de enteros (Grid = [[Int]])
  , prInitialEnergy :: Energy -- la energía inicial (alias Int)
  } deriving (Eq, Show, Generic)

instance FromJSON PathRequest

-- | JSON de salida si hay camino válido
data PathResponse = PathResponse
  { prPath        :: [Pos]   -- lista de posiciones [(fila, columna)]
  , prFinalEnergy :: Energy  -- energía restante al final
  } deriving (Eq, Show, Generic)

instance ToJSON PathResponse

--------------------------------------------------------------------------------
-- 2) Definimos la API: solo un POST /findPath que recibe PathRequest y devuelve PathResponse
--------------------------------------------------------------------------------

type PathFinderAPI =
       "findPath" :> ReqBody '[JSON] PathRequest :> Post '[JSON] PathResponse

-- | Implementación del handler para el endpoint
serverHandler :: PathRequest -> Handler PathResponse
serverHandler (PathRequest grid initEnergy) =
  case bestPath grid initEnergy of
    Nothing          ->
      -- Si no hay un camino válido, respondemos con 400 Bad Request y un mensaje
      throwError err400 { Servant.errBody = "No hay un camino válido con energía ≥ 0." }
    Just (ruta, fe) -> do
      liftIO $ putStrLn $ "Ruta encontrada: " ++ show ruta ++ ", Energía final: " ++ show fe
      return $ PathResponse ruta fe

-- | Construye el servidor Servant a partir de la definición y el handler
pathFinderServer :: Server PathFinderAPI
pathFinderServer = serverHandler

apiProxy :: Proxy PathFinderAPI
apiProxy = Proxy

--------------------------------------------------------------------------------
-- 3) Aplicación Wai que envuelve el servidor con CORS “*” para POST
--------------------------------------------------------------------------------

-- Permitimos CORS desde cualquier origen (corsOrigins = Nothing),
-- métodos POST y el header “Content-Type”.
app :: Application
app = cors (const $ Just policy) $ serve apiProxy pathFinderServer
  where
    policy :: CorsResourcePolicy
    policy = simpleCorsResourcePolicy
      { corsOrigins        = Nothing        -- Nothing ≡ "*"
      , corsMethods        = ["POST"]
      , corsRequestHeaders = ["Content-Type"]
      }

--------------------------------------------------------------------------------
-- 4) Función que arranca el servidor en el puerto dado
--------------------------------------------------------------------------------

-- | runServer toma un Int (puerto) y levanta Warp con la app Servant
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Arrancando PathFinder API en el puerto " ++ show port
  run port app
