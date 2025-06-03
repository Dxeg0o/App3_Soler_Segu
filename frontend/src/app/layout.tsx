import "./globals.css";
import type { ReactNode } from "react";

export const metadata = {
  title: "Magical Runes Forest - PathFinder Game",
  description: "Find the optimal energy path through the magical forest",
};

export default function RootLayout({ children }: { children: ReactNode }) {
  return (
    <html lang="en">
      <body className="bg-gradient-to-br from-purple-50 via-blue-50 to-indigo-50 min-h-screen">
        {children}
      </body>
    </html>
  );
}
