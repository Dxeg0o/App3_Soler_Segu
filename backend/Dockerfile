# ======================= FASE 1: build =======================
FROM haskell:9.2.7 AS builder

WORKDIR /app
COPY . .

RUN cabal update \
 && cabal install --only-dependencies \
 && cabal build --ghc-options="-O2"

# ====================== FASE 2: runtime ======================
FROM debian:bullseye-slim

# Instalamos librerías necesarias para el binario de GHC
RUN apt-get update \
 && apt-get install -y libgmp10 ca-certificates \
 && rm -rf /var/lib/apt/lists/*

# Copiamos el ejecutable compilado por Cabal (como root)
# Ajustamos el "wildcard" con 3 niveles para llegar al paquete+versión
COPY --from=builder \
  /app/dist-newstyle/build/*/*/*/x/App3-Soler-Segu/build/App3-Soler-Segu/App3-Soler-Segu \
  /usr/local/bin/App3-Soler-Segu

# Nos aseguramos de que tenga permiso de ejecución
RUN chmod +x /usr/local/bin/App3-Soler-Segu

# Ahora sí creamos el usuario "appuser" y cambiamos a él
RUN useradd -m appuser
USER appuser

WORKDIR /home/appuser

# Configuración de variables de entorno
ENV PORT=8080
ENV HOST=0.0.0.0

# Usamos shell form para que ${PORT} se expanda correctamente
CMD /usr/local/bin/App3-Soler-Segu --server "${PORT}"
