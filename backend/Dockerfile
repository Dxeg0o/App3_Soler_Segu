# =============== Fase 1: build ==================
FROM haskell:9.2.7 AS builder
WORKDIR /app

# Copiamos todo el código fuente
COPY . .

# Actualizamos índices y resolvemos dependencias
# Si usas Cabal:
RUN cabal update && \
    cabal install --only-dependencies

# Compilamos en modo optimizado (-O2)
RUN cabal build --ghc-options="-O2"

# =============== Fase 2: runtime ==================
FROM debian:bullseye-slim

# Instalamos librerías necesarias (libgmp es requisito para binarios GHC)
RUN apt-get update && \
    apt-get install -y libgmp10 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Creamos un usuario no‐root opcionalmente
RUN useradd -m appuser
USER appuser
WORKDIR /home/appuser

# Copiamos el ejecutable desde la etapa "builder"
# Ajusta la ruta al binario según dónde Cabal lo haya dejado. 
# Normalmente será algo como: dist-newstyle/build/<ghc-version>/<pkg-name>/x/<exe-name>/build/<exe-name>/<exe-name>
COPY --from=builder \
  /app/dist-newstyle/build/*/*/x/App3-Soler-Segu/build/App3-Soler-Segu/App3-Soler-Segu \
  /usr/local/bin/App3

CMD ["bash", "-c", "App3 --server ${PORT:-8080}"]
