{-# OPTIONS_GHC -Wall #-}
module PathFinder
  ( bestPath
  ) where

import Types           (Grid, Energy, PathResult)
import Grid            (gridSize, getStartPosition, getEndPosition, valueAt)
import Rules           (trapPenalty)
import SearchAlgorithm (findAllPaths, selectBestPath)

-- | Valida si el estado inicial es válido (energía >= 0 en la posición inicial)
validateInitialState :: Grid -> Energy -> Maybe Energy
validateInitialState grid initEnergy
  | n == 0 = Nothing
  | startEnergy < 0 = Nothing
  | otherwise = Just startEnergy
  where
    n = gridSize grid
    startPos = getStartPosition
    startValue = valueAt grid startPos
    startPenalty = trapPenalty startValue
    startEnergy = initEnergy + startValue - startPenalty

-- | Calcula el mejor camino que llegue a (n-1, n-1). 
--   Devuelve 'Nothing' si no hay ninguno con energía ≥ 0, o 'Just (ruta, energía)'.
bestPath :: Grid -> Energy -> Maybe PathResult
bestPath grid initEnergy = do
  validStartEnergy <- validateInitialState grid initEnergy
  let startPos = getStartPosition
      endPos = getEndPosition grid
      allPaths = findAllPaths grid startPos endPos validStartEnergy
  selectBestPath allPaths