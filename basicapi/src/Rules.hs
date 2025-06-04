{-# OPTIONS_GHC -Wall #-}
module Rules
  ( trapPenalty
  , moveCost
  , isDiagonal
  , calculateNewEnergy
  , isValidMove
  ) where

import Types (Pos, Energy, Grid)
import Grid  (valueAt)

-- | Si la runa vale 0, es trampa ⇒ penaliza 3 unidades extra; en otro caso 0.
trapPenalty :: Int -> Int
trapPenalty v
  | v == 0    = 3
  | otherwise = 0

-- | El costo extra al moverse entre dos posiciones:
--   • 2 si es diagonal, 0 si es horizontal o vertical
moveCost :: Pos -> Pos -> Int
moveCost a b
  | isDiagonal a b = 2
  | otherwise      = 0

-- | Determina si el movimiento entre dos posiciones es diagonal (abajo-derecha)
isDiagonal :: Pos -> Pos -> Bool
isDiagonal (i1, j1) (i2, j2) = (i2 == i1 + 1) && (j2 == j1 + 1)

-- | Calcula la nueva energía después de moverse a una posición
calculateNewEnergy :: Grid -> Pos -> Pos -> Energy -> Energy
calculateNewEnergy grid currentPos newPos currentEnergy =
  let runeValue = valueAt grid newPos
      movementCost = moveCost currentPos newPos
      trapCost = trapPenalty runeValue
  in currentEnergy - movementCost + runeValue - trapCost

-- | Verifica si un movimiento es válido (energía >= 0)
isValidMove :: Grid -> Pos -> Pos -> Energy -> Bool
isValidMove grid currentPos newPos currentEnergy =
  calculateNewEnergy grid currentPos newPos currentEnergy >= 0