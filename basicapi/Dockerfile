# ---- Etapa 1: construcción con la imagen oficial de Haskell ----
    FROM haskell:9.2 AS build

    # Directorio de trabajo
    WORKDIR /app
    
    # Copiamos .cabal y el código fuente
    COPY basicapi.cabal /app/
    COPY app /app/app
    
    # Actualizamos repos y compilamos el ejecutable
    RUN cabal update
    RUN cabal build --enable-relocatable
    
    # ---- Etapa 2: imagen mínima de producción ----
    FROM debian:buster-slim
    
    # Instalamos runtime necesario (GMP)
    RUN apt-get update \
     && apt-get install -y libgmp10 \
     && rm -rf /var/lib/apt/lists/*
    
    WORKDIR /app
    
    # Copiamos el binario compilado en la etapa anterior.
    # Ajusta la ruta si tu versión de GHC/cabal difiere.
    COPY --from=build \
      /app/dist-newstyle/build/x86_64-linux/ghc-9.2.*/basicapi-0.1.0.0/x/basicapi-exe/build/basicapi-exe/basicapi-exe \
      /app/basicapi-exe
    
    # Exponemos el puerto 8080 (Render enlaza a PORT)
    EXPOSE 8080
    
    # Comando por defecto para ejecutar
    CMD ["/app/basicapi-exe"]
    