{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Server (runServer) where

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
-- Aquí están todos los símbolos de Servant que vamos a usar:
import           Servant
  ( Handler
  , Server
  , serve
  , (:<|>)(..)
  , (:>)
  , ReqBody
  , JSON
  , Post
  )
import qualified Servant as S

import           Types                     (Grid, Energy, Pos)
import           PathFinder                (bestPath)

--------------------------------------------------------------------------------
-- 1) TIPOS JSON
--------------------------------------------------------------------------------

-- | Lo que el cliente envía a /findPath
data PathRequest = PathRequest
  { prGrid          :: Grid
  , prInitialEnergy :: Energy
  } deriving (Eq, Show, Generic)

instance FromJSON PathRequest

-- | En caso de éxito: camino y energía final
data PathResponse = PathResponse
  { prPath        :: [Pos]
  , prFinalEnergy :: Energy
  } deriving (Eq, Show, Generic)

instance ToJSON PathResponse

-- | En caso de error: sólo un mensaje de texto
data ErrorResponse = ErrorResponse
  { errMsg :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON ErrorResponse

--------------------------------------------------------------------------------
-- 2) LA API: POST /findPath devuelve Either ErrorResponse PathResponse
--------------------------------------------------------------------------------

type PathFinderAPI =
       "findPath"
    :> ReqBody '[JSON] PathRequest
    :> Post '[JSON] (Either ErrorResponse PathResponse)

apiProxy :: Proxy PathFinderAPI
apiProxy = Proxy

--------------------------------------------------------------------------------
-- 3) HANDLER: devuelve Left ErrorResponse o Right PathResponse
--------------------------------------------------------------------------------

serverHandler :: PathRequest -> Handler (Either ErrorResponse PathResponse)
serverHandler (PathRequest grid initEnergy) = do
  liftIO $ putStrLn $ "Recibido POST /findPath con grid=" ++ show grid ++ " y energía=" ++ show initEnergy
  case bestPath grid initEnergy of
    Nothing ->
      pure $ Left $ ErrorResponse "No hay un camino válido con la energía proporcionada."
    Just (ruta, eFinal) -> do
      liftIO $ putStrLn $ "Ruta encontrada: " ++ show ruta ++ ", energía final: " ++ show eFinal
      pure $ Right $ PathResponse ruta eFinal

pathFinderServer :: Server PathFinderAPI
pathFinderServer = serverHandler

--------------------------------------------------------------------------------
-- 4) CORS “*” y SERVANT
--------------------------------------------------------------------------------

app :: Application
app = cors (const $ Just policy) $ serve apiProxy pathFinderServer
  where
    policy :: CorsResourcePolicy
    policy = simpleCorsResourcePolicy
      { corsOrigins        = Nothing       -- Nothing ≡ Access-Control-Allow-Origin: *
      , corsMethods        = ["POST"]
      , corsRequestHeaders = ["Content-Type"]
      }

--------------------------------------------------------------------------------
-- 5) runServer que arranca Warp en el puerto dado
--------------------------------------------------------------------------------

runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Arrancando PathFinder API en el puerto " ++ show port
  run port app
