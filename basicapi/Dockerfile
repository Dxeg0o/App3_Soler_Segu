# -------------------------------------------------------------------
# Etapa 1: Construcción con GHC/Cabal
# -------------------------------------------------------------------
    FROM haskell:9.2 AS build

    WORKDIR /app
    
    # 1) Copiamos el cabal file y las carpetas src/ y app/
    COPY basicapi.cabal /app/
    COPY src /app/src
    COPY app /app/app
    
    # 2) Actualizamos índices y compilamos el ejecutable, 
    #    colocando el binario resultante en /app/basicapi-exe
    RUN cabal update \
     && cabal v2-install exe:basicapi-exe \
           --installdir=/app \
           --install-method=copy
    
    # -------------------------------------------------------------------
    # Etapa 2: Imagen de producción “slim”
    # -------------------------------------------------------------------
    FROM debian:buster-slim
    
    # 3) Instalamos solo la dependencia nativa de GMP (necesaria para binarios GHC)
    RUN apt-get update \
     && apt-get install -y libgmp10 \
     && rm -rf /var/lib/apt/lists/*
    
    WORKDIR /app
    
    # 4) Copiamos el ejecutable que ya se generó en la etapa “build”
    COPY --from=build /app/basicapi-exe /app/basicapi-exe
    
    # 5) Exponemos el puerto 8080 (Render vincula automáticamente PORT a 8080)
    EXPOSE 8080
    
    # 6) Arrancamos el binario al iniciar el contenedor
    CMD ["/app/basicapi-exe", "--server"]
    