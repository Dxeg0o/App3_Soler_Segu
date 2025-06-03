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
      <body className="bg-gray-50">
        <div className="min-h-screen flex flex-col items-center px-4 py-8">
          {children}
        </div>
      </body>
    </html>
  );
}
