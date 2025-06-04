# Etapa 1: Construcción
FROM haskell:9.2 AS build

# Directorio de trabajo dentro del contenedor
WORKDIR /app

# Copiamos el archivo .cabal y las fuentes
COPY basicapi.cabal /app/
COPY app /app/app

# Actualizamos índices y compilamos
RUN cabal update
RUN cabal build --enable-relocatable

# Etapa 2: Imagen de producción
FROM debian:buster-slim

# Dependencia de GMP (necesaria para binarios de GHC)
RUN apt-get update \
 && apt-get install -y libgmp10 \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copiamos el ejecutable desde la etapa de build
# (La ruta exacta puede variar según la versión de GHC/cabal;
#  aquí asumimos dist-newstyle/build/x86_64-linux/ghc-9.2.*/basicapi-*/x/basicapi-exe/build/basicapi-exe/basicapi-exe)
COPY --from=build \
  /app/dist-newstyle/build/x86_64-linux/ghc-9.2.*/*/x/basicapi-exe/build/basicapi-exe/basicapi-exe \
  /app/basicapi-exe

# Exponemos el puerto 8080 (Render vincula PORT a 8080)
EXPOSE 8080

# Comando por defecto al iniciar el contenedor
CMD ["/app/basicapi-exe"]
