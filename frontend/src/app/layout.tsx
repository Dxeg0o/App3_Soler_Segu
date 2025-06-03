import "./globals.css";
import type { ReactNode } from "react";

export const metadata = {
  title: "Bosque de Runas Mágicas - Juego de Búsqueda de Ruta",
  description: "Encuentra la ruta de energía óptima a través del bosque mágico",
};

export default function RootLayout({ children }: { children: ReactNode }) {
  return (
    <html lang="es">
      <body className="bg-gradient-to-br from-purple-50 via-blue-50 to-indigo-50 min-h-screen">
        {children}
      </body>
    </html>
  );
}
