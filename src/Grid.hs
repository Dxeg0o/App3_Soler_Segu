-- src/Grid.hs
{-# OPTIONS_GHC -Wall #-}
module Grid
  ( Pos
  , Grid
  , gridSize
  , inBounds
  , valueAt
  , isDiagonal
  , neighbors3Dir
  , trapPenalty
  , moveCost
  ) where

-- | Posición en el bosque (fila, columna)
type Pos = (Int, Int)

-- | Una matriz de enteros representa el bosque de runas
type Grid = [[Int]]

-- | Obtiene el tamaño (n) de una matriz cuadrada NxN
gridSize :: Grid -> Int
gridSize g = length g

-- | Verifica si una posición está dentro de los límites 0..(n-1)
inBounds :: Int -> Pos -> Bool
inBounds n (i, j) = i >= 0 && j >= 0 && i < n && j < n

-- | Obtiene el valor de la runa en la posición dada
valueAt :: Grid -> Pos -> Int
valueAt g (i, j) = (g !! i) !! j

-- | Determina si el movimiento entre dos posiciones es diagonal (abajo-derecha)
isDiagonal :: Pos -> Pos -> Bool
isDiagonal (i1, j1) (i2, j2) = (i2 == i1 + 1) && (j2 == j1 + 1)

-- | Genera las tres direcciones: derecha, abajo y diagonal ⤵
--   (sólo esas tres, para evitar explosión combinatoria)
neighbors3Dir :: Int  -- ^ tamaño n de la matriz
              -> Pos  -- ^ posición actual
              -> [Pos]-- ^ posiciones candidatas (solo tres direcciones)
neighbors3Dir n (i, j) =
  filter (inBounds n)
    [ (i,     j + 1)  -- derecha
    , (i + 1, j    )  -- abajo
    , (i + 1, j + 1)  -- diagonal
    ]

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
