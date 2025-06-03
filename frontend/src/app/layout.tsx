// src/app/layout.tsx
import "../app/globals.css";
import { ReactNode } from "react";

export const metadata = {
  title: "PathFinder Frontend",
  description: "Interfaz para consumir PathFinder API",
};

export default function RootLayout({ children }: { children: ReactNode }) {
  return (
    <html lang="es">
      <body className="bg-gradient-to-br from-indigo-50 to-white">
        <div className="min-h-screen flex flex-col items-center px-4 py-10">
          {children}
        </div>
      </body>
    </html>
  );
}
