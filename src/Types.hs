{-# OPTIONS_GHC -Wall #-}
module Types
  ( Pos
  , Grid
  , Energy
  , Path
  , PathResult
  , GameState(..)
  ) where

-- | Posición en el bosque (fila, columna)
type Pos = (Int, Int)

-- | Una matriz de enteros representa el bosque de runas
type Grid = [[Int]]

-- | Energía del jugador
type Energy = Int

-- | Camino como lista de posiciones
type Path = [Pos]

-- | Resultado de búsqueda de camino: (ruta, energía final)
type PathResult = (Path, Energy)

-- | Estado completo del juego durante la búsqueda
data GameState = GameState
  { gsPosition :: Pos     -- ^ posición actual
  , gsEnergy   :: Energy  -- ^ energía actual
  , gsPath     :: Path    -- ^ camino recorrido
  , gsVisited  :: [Pos]   -- ^ posiciones visitadas
  } deriving (Show, Eq)