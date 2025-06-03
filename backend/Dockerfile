# ======================= FASE 1: build =======================
FROM haskell:9.2.7 AS builder

# Directorio de trabajo dentro del builder
WORKDIR /app

# Copiamos todo el código fuente del proyecto a /app
COPY . .

# Actualizamos índices y resolvemos dependencias
RUN cabal update \
 && cabal install --only-dependencies

# Compilamos en modo optimizado (-O2)
RUN cabal build --ghc-options="-O2"

# ====================== FASE 2: runtime ======================
FROM debian:bullseye-slim

# Instalamos librerías necesarias (libgmp es requisito para binarios GHC)
RUN apt-get update \
 && apt-get install -y libgmp10 ca-certificates \
 && rm -rf /var/lib/apt/lists/*

# Creamos un usuario no-root opcionalmente
RUN useradd -m appuser

# Cambiamos al usuario sin privilegios
USER appuser

# Directorio de trabajo para la fase runtime
WORKDIR /home/appuser

# Copiamos el ejecutable desde la etapa "builder".
# Obs.: Ajustamos el wildcard para que coincida con el paquete+versión.
#      Y lo copiamos con el nombre “App3-Soler-Segu” a /usr/local/bin.
COPY --from=builder \
  /app/dist-newstyle/build/*/*/*/x/App3-Soler-Segu/build/App3-Soler-Segu/App3-Soler-Segu \
  /usr/local/bin/App3-Soler-Segu

# Aseguramos permisos de ejecución (por si algo quedase mal)
RUN chmod +x /usr/local/bin/App3-Soler-Segu

# Al usar shell form, Docker expandirá ${PORT:-8080}. Si no existe $PORT, usará 8080.
CMD /usr/local/bin/App3-Soler-Segu --server "${PORT:-8080}"
