{-# OPTIONS_GHC -Wall #-}
module SearchAlgorithm
  ( findAllPaths
  , selectBestPath
  ) where

import Data.List (maximumBy)
import Data.Ord  (comparing)
import Types     (Pos, Grid, Energy, Path, PathResult, GameState(..))
import Grid      (gridSize, neighbors3Dir)
import Rules     (calculateNewEnergy, isValidMove)

-- | Estado inicial del juego
initialState :: Pos -> Energy -> GameState
initialState startPos startEnergy = GameState
  { gsPosition = startPos
  , gsEnergy   = startEnergy
  , gsPath     = [startPos]
  , gsVisited  = [startPos]
  }

-- | DFS recursivo que explora todos los caminos posibles (sin re-visitar).
--   Cuando llega a 'objetivo', devuelve [(camino, energíaFinal)]. 
dfsSearch :: Grid         -- ^ la matriz de runas
          -> Pos          -- ^ posición final (objetivo)
          -> GameState    -- ^ estado actual del juego
          -> [PathResult] -- ^ lista de resultados (camino, energía final)
dfsSearch grid objetivo state
  | gsPosition state == objetivo = [(reverse (gsPath state), gsEnergy state)]
  | otherwise = concatMap tryMove validMoves
  where
    n = gridSize grid
    currentPos = gsPosition state
    currentEnergy = gsEnergy state
    nextPositions = filter (`notElem` gsVisited state) (neighbors3Dir n currentPos)
    
    -- Fix: Filter positions, not energies, and pass parameters in correct order
    validMoves = filter (\pos -> isValidMove grid currentPos pos currentEnergy) nextPositions
    
    tryMove :: Pos -> [PathResult]
    tryMove newPos =
      let newEnergy = calculateNewEnergy grid currentPos newPos currentEnergy
          newState = GameState
            { gsPosition = newPos
            , gsEnergy   = newEnergy
            , gsPath     = newPos : gsPath state
            , gsVisited  = newPos : gsVisited state
            }
      in dfsSearch grid objetivo newState

-- | Encuentra todos los caminos válidos desde una posición inicial
findAllPaths :: Grid -> Pos -> Pos -> Energy -> [PathResult]
findAllPaths grid startPos endPos startEnergy =
  let initState = initialState startPos startEnergy
  in dfsSearch grid endPos initState

-- | Selecciona el mejor camino basado en la energía final
selectBestPath :: [PathResult] -> Maybe PathResult
selectBestPath [] = Nothing
selectBestPath results = Just $ maximumBy (comparing snd) results