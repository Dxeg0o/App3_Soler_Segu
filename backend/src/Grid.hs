{-# OPTIONS_GHC -Wall #-}
module Grid
  ( gridSize
  , inBounds
  , valueAt
  , neighbors3Dir
  , getStartPosition
  , getEndPosition
  ) where

import Types (Pos, Grid)

-- | Obtiene el tamaño (n) de una matriz cuadrada NxN
gridSize :: Grid -> Int
gridSize g = length g

-- | Verifica si una posición está dentro de los límites 0..(n-1)
inBounds :: Int -> Pos -> Bool
inBounds n (i, j) = i >= 0 && j >= 0 && i < n && j < n

-- | Obtiene el valor de la runa en la posición dada
valueAt :: Grid -> Pos -> Int
valueAt g (i, j) = (g !! i) !! j

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

-- | Obtiene la posición inicial del juego
getStartPosition :: Pos
getStartPosition = (0, 0)

-- | Obtiene la posición final del juego basada en el tamaño del grid
getEndPosition :: Grid -> Pos
getEndPosition grid = 
  let n = gridSize grid
  in (n - 1, n - 1)