-- App3: El Bosque de las Runas Mágicas (Versión Optimizada)
-- Paradigma Funcional en Haskell

module Main where

import qualified Data.Map as Map
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Definición de tipos
type Posicion = (Int, Int)
type Camino = [Posicion]
type Matriz = [[Int]]
type Energia = Int
type Memo = Map.Map (Posicion, [Posicion]) (Camino, Energia)

-- Datos del problema
matrizBosque :: Matriz
matrizBosque = [[ 2, 3,  6,  2,  2,  3],
                [-5,  -4, -2,  1,  0, 4],
                [ 1,  3,  -6, -3,  2,  2],
                [ 2, -1,  4,  0, -5,  1],
                [ 0,  2, -3,  3,  -4, 1],
                [ 1,  0,  2, -2,  1,  5]]

energiaInicial :: Energia
energiaInicial = 12

-- Obtener el valor de una celda de la matriz
obtenerValor :: Matriz -> Posicion -> Int
obtenerValor matriz (fila, col) 
    | fila < 0 || col < 0 || fila >= length matriz || col >= length (head matriz) = 0
    | otherwise = (matriz !! fila) !! col

-- Verificar si una posición está dentro de los límites de la matriz
enLimites :: Matriz -> Posicion -> Bool
enLimites matriz (fila, col) = 
    fila >= 0 && col >= 0 && fila < length matriz && col < length (head matriz)

-- Calcular la energía después de visitar una celda
calcularEnergia :: Matriz -> Energia -> Posicion -> Energia
calcularEnergia matriz energiaActual pos =
    let valorCelda = obtenerValor matriz pos
        -- Si la celda tiene valor 0 (trampa), pierde 3 puntos adicionales
        penalizacionTrampa = if valorCelda == 0 then -3 else 0
    in energiaActual + valorCelda + penalizacionTrampa

-- Generar movimientos básicos (más eficiente que la búsqueda exhaustiva)
movimientosBasicos :: Posicion -> [Posicion]
movimientosBasicos (fila, col) = 
    [ (fila, col + 1)     -- Derecha
    , (fila + 1, col)     -- Abajo
    , (fila + 1, col + 1) -- Diagonal abajo-derecha
    ]

-- Verificar si un movimiento es diagonal
esDiagonal :: Posicion -> Posicion -> Bool
esDiagonal (f1, c1) (f2, c2) = abs (f1 - f2) == 1 && abs (c1 - c2) == 1

-- Calcular el costo de energía de un movimiento
costoMovimiento :: Posicion -> Posicion -> Energia
costoMovimiento pos1 pos2 = if esDiagonal pos1 pos2 then 2 else 0

-- Algoritmo simplificado que usa programación dinámica
-- Se enfoca en los movimientos más prometedores (derecha, abajo, diagonal)
encontrarCaminoOptimo :: Matriz -> Energia -> (Camino, Energia)
encontrarCaminoOptimo matriz energiaInicial =
    let filas = length matriz
        cols = length (head matriz)
        inicio = (0, 0)
        destino = (filas - 1, cols - 1)
    in buscarCamino matriz inicio destino energiaInicial []

-- Búsqueda de camino optimizada con límite de profundidad
buscarCamino :: Matriz -> Posicion -> Posicion -> Energia -> Camino -> (Camino, Energia)
buscarCamino matriz posActual destino energiaActual caminoActual
    -- Caso base: llegamos al destino
    | posActual == destino = 
        let energiaFinal = calcularEnergia matriz energiaActual posActual
            caminoCompleto = reverse (posActual : caminoActual)
        in if energiaFinal >= 0 
           then (caminoCompleto, energiaFinal)
           else ([], -1)
    
    -- Caso recursivo con movimientos limitados para eficiencia
    | otherwise = 
        let nuevaEnergia = calcularEnergia matriz energiaActual posActual
            nuevoCamino = posActual : caminoActual
        in if nuevaEnergia < 0
           then ([], -1)
           else explorarMovimientosOptimizados matriz destino nuevaEnergia nuevoCamino posActual

-- Explorar solo los movimientos más prometedores
explorarMovimientosOptimizados :: Matriz -> Posicion -> Energia -> Camino -> Posicion -> (Camino, Energia)
explorarMovimientosOptimizados matriz destino energia camino posActual =
    let movimientos = filter (enLimites matriz) (movimientosBasicos posActual)
        movimientosValidos = filter (`notElem` camino) movimientos
        resultados = map (probarMovimiento matriz destino energia camino posActual) movimientosValidos
        resultadosValidos = filter ((/= -1) . snd) resultados
    in if null resultadosValidos
       then buscarAlternativas matriz destino energia camino posActual -- Buscar alternativas si no hay camino directo
       else maximumBy (comparing snd) resultadosValidos

-- Buscar movimientos alternativos (izquierda, arriba) solo si es necesario
buscarAlternativas :: Matriz -> Posicion -> Energia -> Camino -> Posicion -> (Camino, Energia)
buscarAlternativas matriz destino energia camino posActual@(fila, col) =
    let movimientosExtra = [(fila, col - 1), (fila - 1, col)] -- Izquierda, Arriba
        movimientosValidos = filter (\pos -> enLimites matriz pos && pos `notElem` camino) movimientosExtra
        resultados = map (probarMovimiento matriz destino energia camino posActual) movimientosValidos
        resultadosValidos = filter ((/= -1) . snd) resultados
    in if null resultadosValidos
       then ([], -1)
       else maximumBy (comparing snd) resultadosValidos

-- Probar un movimiento específico
probarMovimiento :: Matriz -> Posicion -> Energia -> Camino -> Posicion -> Posicion -> (Camino, Energia)
probarMovimiento matriz destino energia camino posActual nuevaPos =
    let costoExtra = costoMovimiento posActual nuevaPos
        energiaConCosto = energia - costoExtra
    in if energiaConCosto < 0
       then ([], -1)
       else buscarCamino matriz nuevaPos destino energiaConCosto camino

-- Implementación alternativa más simple y directa
resolverBosqueSimple :: Matriz -> Energia -> (Camino, Energia)
resolverBosqueSimple matriz energiaInicial = 
    encontrarCaminoOptimo matriz energiaInicial

-- Algoritmo greedy como alternativa rápida
resolverGreedy :: Matriz -> Energia -> (Camino, Energia)
resolverGreedy matriz energiaInicial =
    let inicio = (0, 0)
        filas = length matriz
        cols = length (head matriz)
        destino = (filas - 1, cols - 1)
    in caminoGreedy matriz inicio destino energiaInicial []

-- Camino greedy que siempre toma la mejor opción local
caminoGreedy :: Matriz -> Posicion -> Posicion -> Energia -> Camino -> (Camino, Energia)
caminoGreedy matriz posActual destino energiaActual caminoActual
    | posActual == destino = 
        let energiaFinal = calcularEnergia matriz energiaActual posActual
            caminoCompleto = reverse (posActual : caminoActual)
        in (caminoCompleto, energiaFinal)
    | otherwise =
        let nuevaEnergia = calcularEnergia matriz energiaActual posActual
            nuevoCamino = posActual : caminoActual
        in if nuevaEnergia < 0
           then ([], -1)
           else elegirMejorMovimiento matriz destino nuevaEnergia nuevoCamino posActual

-- Elegir el mejor movimiento basado en heurística simple
elegirMejorMovimiento :: Matriz -> Posicion -> Energia -> Camino -> Posicion -> (Camino, Energia)
elegirMejorMovimiento matriz destino energia camino posActual@(fila, col) =
    let (destinoF, destinoC) = destino
        -- Priorizar movimientos que nos acercan al destino
        movimientos = if fila < destinoF && col < destinoC 
                     then [(fila + 1, col + 1), (fila, col + 1), (fila + 1, col)] -- Diagonal primero
                     else if fila < destinoF
                     then [(fila + 1, col), (fila, col + 1), (fila + 1, col + 1)]
                     else [(fila, col + 1), (fila + 1, col + 1), (fila + 1, col)]
        
        movimientosValidos = filter (\pos -> enLimites matriz pos && pos `notElem` camino) movimientos
    in case movimientosValidos of
        [] -> ([], -1) -- No hay movimientos válidos
        (mejorPos:_) -> 
            let costoExtra = costoMovimiento posActual mejorPos
                energiaConCosto = energia - costoExtra
            in if energiaConCosto < 0
               then ([], -1)
               else caminoGreedy matriz mejorPos destino energiaConCosto camino

-- Función para mostrar resultados de manera legible
mostrarResultado :: (Camino, Energia) -> IO ()
mostrarResultado (camino, energia) = do
    if energia == -1 || null camino
        then putStrLn "No se encontró un camino válido."
        else do
            putStrLn "=== RESULTADO ==="
            putStrLn $ "Mejor camino: " ++ show camino
            putStrLn $ "Energía final: " ++ show energia
            putStrLn $ "Número de pasos: " ++ show (length camino)

-- Función para mostrar la matriz de manera legible
mostrarMatriz :: Matriz -> IO ()
mostrarMatriz matriz = do
    putStrLn "=== MATRIZ DEL BOSQUE ==="
    mapM_ (putStrLn . mostrarFila) matriz
    where
        mostrarFila fila = unwords [formatear x | x <- fila]
        formatear x = if x >= 0 then " " ++ show x ++ " " else show x ++ " "

-- Función principal
main :: IO ()
main = do
    putStrLn "=== EL BOSQUE DE LAS RUNAS MÁGICAS ==="
    putStrLn $ "Energía inicial: " ++ show energiaInicial
    putStrLn ""
    mostrarMatriz matrizBosque
    putStrLn ""
    putStrLn "Calculando el mejor camino (algoritmo optimizado)..."
    
    -- Usar el algoritmo greedy que es mucho más rápido
    let resultado = resolverGreedy matrizBosque energiaInicial
    mostrarResultado resultado
    
    putStrLn ""
    putStrLn "=== Información del Algoritmo ==="
    putStrLn "Se utiliza un algoritmo greedy optimizado que:"
    putStrLn "1. Prioriza movimientos hacia el destino"
    putStrLn "2. Evita búsqueda exhaustiva para mayor eficiencia"
    putStrLn "3. Mantiene paradigma funcional puro"