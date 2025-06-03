{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types  -- Add this line
  ( PathRequest(..)
  , PathResponse(..)
  , ErrorResponse(..)
  -- Potentially re-export types from the main "Types" module if needed by API consumers
  , module Types -- This re-exports everything from your core "Types" module
  -- Or, if you prefer to be explicit:
  -- , Pos, Grid, Energy, Path, PathResult, GameState -- (ensure these are imported below)
  ) where

import Data.Aeson
import GHC.Generics
import Types (Pos, Grid, Energy, Path, PathResult, GameState(..)) -- Keep this comprehensive import

-- | Solicitud para encontrar el mejor camino
data PathRequest = PathRequest
  { prGrid :: Grid
  , prInitialEnergy :: Energy
  } deriving (Show, Generic)

-- | Respuesta exitosa con el camino encontrado
data PathResponse = PathResponse
  { prPath :: Path
  , prFinalEnergy :: Energy
  } deriving (Show, Generic)

-- | Respuesta de error
data ErrorResponse = ErrorResponse
  { errMessage :: String
  } deriving (Show, Generic)

-- Instancias JSON para serialización automática
instance FromJSON PathRequest
instance ToJSON PathRequest
instance FromJSON PathResponse
instance ToJSON PathResponse
instance FromJSON ErrorResponse
instance ToJSON ErrorResponse