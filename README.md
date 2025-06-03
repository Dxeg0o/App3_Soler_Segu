# App3 - El Bosque de las Runas Mágicas

Programa desarrollado en Haskell que implementa un algoritmo para encontrar el camino óptimo a través de un bosque encantado lleno de runas mágicas.

## Integrantes del Grupo

- **Nombre:** [Tu nombre]
- **Correo:** [tu.correo@alumnos.uai.cl]
- **Nombre:** [Compañero 1]
- **Correo:** [correo1@alumnos.uai.cl]

## Descripción del Problema

Un mago debe atravesar un bosque encantado desde la esquina superior izquierda hasta la esquina inferior derecha, maximizando su energía restante. El bosque está representado por una matriz donde cada celda contiene una runa con un valor que puede ser positivo o negativo.

### Reglas del Juego

1. **Movimientos permitidos:**

   - Derecha o abajo (siempre válidos)
   - Diagonal abajo-derecha (cuesta 2 puntos extra de energía)
   - Izquierda o arriba (solo si no regresa a una celda ya visitada)

2. **Penalizaciones especiales:**
   - Movimientos diagonales: -2 puntos de energía adicionales
   - Celdas con valor 0 (trampas): -3 puntos de energía adicionales
   - Si la energía llega a ser menor que 0, el camino se invalida

## Compilación y Ejecución

### Prerrequisitos

- GHC (Glasgow Haskell Compiler)
- Cabal o Stack

### Compilación

```bash
# Con Cabal
cabal configure
cabal build

# Con Stack (si tienes stack.yaml)
stack build
```

### Ejecución

```bash
# Ejecutar con ejemplo por defecto
cabal run App3

# Ejecutar con energía específica
cabal run App3 -- 15

# Si compilaste manualmente
./dist/build/App3/App3
```

## Ejemplo de Uso

### Matriz de Ejemplo

```
[[ 2, -3,  1,  0,  2,  3],
 [-5,  4, -2,  1,  0, -4],
 [ 1,  3,  0, -3,  2,  2],
 [ 2, -1,  4,  0, -5,  1],
 [ 0,  2, -3,  3,  4, -1],
 [ 1,  0,  2, -2,  1,  5]]
```

**Energía inicial:** 12

### Salida Esperada

```
Camino óptimo: [(0,0),(0,1),(1,1),(2,1),(2,2),(3,2),(4,2),(5,2),(5,3),(5,4),(5,5)]
Energía final: X
```

## Estructura del Proyecto

```
src/
├── Main.hs                   # Punto de entrada del programa
├── BosqueRunas.hs            # Módulo principal
└── BosqueRunas/
    ├── Types.hs              # Definición de tipos de datos
    ├── Core.hs               # Lógica principal
    ├── Pathfinding.hs        # Algoritmo de búsqueda de caminos
    ├── Movement.hs           # Lógica de movimientos
    ├── Validation.hs         # Validaciones
    └── Utils.hs              # Funciones auxiliares
```

## Paradigma Funcional

El proyecto implementa los principios del paradigma funcional:

- **Funciones puras:** Sin efectos secundarios
- **Inmutabilidad:** No se modifican variables existentes
- **Recursión:** En lugar de bucles iterativos
- **Composición de funciones:** Construcción modular de la solución
- **Estructuras de datos funcionales:** Listas y tipos algebraicos

## Algoritmo

La solución utiliza una búsqueda exhaustiva con poda (DFS) que:

1. Explora todos los caminos posibles desde el origen
2. Valida que cada movimiento sea legal y no reduzca la energía por debajo de 0
3. Encuentra todos los caminos que lleguen al destino
4. Selecciona el camino que maximice la energía final

## Decisiones de Diseño

- **Modularidad:** Separación clara de responsabilidades en módulos
- **Types.hs:** Definición centralizada de tipos para consistencia
- **Validation.hs:** Lógica de validación separada para claridad
- **Maybe types:** Para manejar casos de error de forma funcional
- **Recursión tail-call:** Donde es posible para eficiencia
