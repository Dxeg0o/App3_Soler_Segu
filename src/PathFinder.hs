-- src/PathFinder.hs
{-# OPTIONS_GHC -Wall #-}
module PathFinder
  ( bestPath
  ) where

import Data.List  (maximumBy)
import Data.Ord   (comparing)
import Grid       (Grid, Pos, gridSize, trapPenalty, valueAt, moveCost, neighbors3Dir)

-- | DFS recursivo que explora todos los caminos posibles (sin re-visitar).
--   Cuando llega a 'objetivo', devuelve [(camino, energíaFinal)]. 
dfs :: Grid         -- ^ la matriz de runas
    -> Int          -- ^ tamaño n
    -> Pos          -- ^ posición final (objetivo)
    -> Pos          -- ^ posición actual
    -> [Pos]        -- ^ celdas ya visitadas
    -> Int          -- ^ energía disponible en la posición actual
    -> [Pos]        -- ^ trayectoria acumulada (incluye la posición actual)
    -> [([Pos], Int)]
dfs grid n objetivo pos visited energy pathSoFar
  | pos == objetivo = [(reverse pathSoFar, energy)]
  | otherwise        =
      let nexts = filter (`notElem` visited) (neighbors3Dir n pos)
      in concatMap tryMove nexts
  where
    tryMove :: Pos -> [([Pos], Int)]
    tryMove p' =
      let v       = valueAt grid p'                    -- valor de la runa en p'
          costMv  = moveCost pos p'                     -- penalización si es diagonal
          penTrap = trapPenalty v                       -- si runa == 0
          nuevaE  = energy - costMv + v - penTrap       -- energía al llegar a p'
      in if nuevaE < 0
           then []  -- descartamos camino si energía < 0
           else dfs grid n objetivo p' (p':visited) nuevaE (p':pathSoFar)

-- | Calcula el mejor camino que llegue a (n-1, n-1). 
--   Devuelve 'Nothing' si no hay ninguno con energía ≥ 0, o 'Just (ruta, energía)'.
bestPath :: Grid -> Int -> Maybe ([Pos], Int)
bestPath grid initEnergy
  | n == 0   = Nothing
  | otherwise =
      let objetivo     = (n - 1, n - 1)
          valorStart   = valueAt grid (0, 0)
          penStart     = trapPenalty valorStart
          energyStart  = initEnergy + valorStart - penStart
      in if energyStart < 0
           then Nothing
           else
             let resultados = dfs grid n objetivo (0, 0) [(0, 0)] energyStart [(0, 0)]
             in if null resultados
                  then Nothing
                  else Just $ maximumBy (comparing snd) resultados
  where
    n = gridSize grid
