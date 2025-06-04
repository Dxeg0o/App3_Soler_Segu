// src/components/PathFinderForm.tsx
"use client";

import { useState, FormEvent } from "react";
import { PathRequest, PathResponse, ErrorResponse } from "../types/api";

export default function PathFinderForm() {
  // Estados
  const [gridText, setGridText] = useState<string>("");
  const [energyText, setEnergyText] = useState<string>("");
  const [loading, setLoading] = useState<boolean>(false);
  const [result, setResult] = useState<PathResponse | null>(null);
  const [error, setError] = useState<string | null>(null);

  // Handler al enviar el formulario
  async function handleSubmit(e: FormEvent) {
    e.preventDefault();
    setError(null);
    setResult(null);

    let parsedGrid: number[][] = [];
    let parsedEnergy: number = 0;

    // 1. Intentar parsear la matriz como JSON
    try {
      parsedGrid = JSON.parse(gridText) as number[][];
      if (
        !Array.isArray(parsedGrid) ||
        parsedGrid.length === 0 ||
        !parsedGrid.every(
          (row) => Array.isArray(row) && row.every((v) => typeof v === "number")
        )
      ) {
        throw new Error(
          "La matriz debe ser un array bidimensional de números."
        );
      }
    } catch {
      setError("Error al parsear la matriz: asegúrate de que sea JSON válido.");
      return;
    }

    // 1.a Verificar que la matriz sea cuadrada (N x N)
    const n = parsedGrid.length;
    const esCuadrada = parsedGrid.every((fila) => fila.length === n);
    if (!esCuadrada) {
      setError(
        "La matriz debe ser cuadrada (por ejemplo, 3×3, 4×4, 5×5, etc.)."
      );
      return;
    }

    // 2. Intentar parsear la energía inicial
    parsedEnergy = parseInt(energyText);
    if (isNaN(parsedEnergy)) {
      setError("La energía inicial debe ser un número entero.");
      return;
    }

    setLoading(true);

    try {
      const baseUrl =
        process.env.NEXT_PUBLIC_API_BASE_URL ?? "http://localhost:8000";
      const resp = await fetch(`${baseUrl}/api/findPath`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          prGrid: parsedGrid,
          prInitialEnergy: parsedEnergy,
        } as PathRequest),
      });

      if (!resp.ok) {
        throw new Error(`HTTP error: ${resp.status}`);
      }

      const json = await resp.json();
      if ("Right" in json) {
        setResult(json.Right as PathResponse);
      } else if ("Left" in json) {
        setError((json.Left as ErrorResponse).errMessage);
      } else if ("errMessage" in json) {
        setError((json as ErrorResponse).errMessage);
      } else {
        setResult(json as PathResponse);
      }
    } catch (err) {
      console.error(err);
      setError("Error al conectar con la API.");
    } finally {
      setLoading(false);
    }
  }

  return (
    <section className="w-full max-w-2xl bg-white/80 backdrop-blur-md shadow-lg rounded-xl p-8 space-y-6">
      <h2 className="text-2xl font-bold text-indigo-700">Simulador de Camino</h2>
      <form onSubmit={handleSubmit} className="space-y-5">
        {/* Textarea para la matriz */}
        <div>
          <label htmlFor="grid" className="block text-sm font-medium text-gray-700 mb-1">
            Matriz (JSON)
          </label>
          <p className="text-xs text-gray-500 mb-1">
            Usa un formato como
            <code className="px-1">[[1,-2,3],[0,4,-1],[2,2,5]]</code>
          </p>
          <textarea
            id="grid"
            rows={5}
            value={gridText}
            onChange={(e) => setGridText(e.target.value)}
            className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500 sm:text-sm"
            placeholder="[[2,-1,0,3,-2],[1,4,-3,2,0],[0,2,1,-4,3],[3,-2,2,1,-1],[1,0,-2,5,2]]"
          />
        </div>

        {/* Input para energía inicial */}
        <div>
          <label
            htmlFor="energy"
            className="block text-sm font-medium text-gray-700 mb-1"
          >
            Energía Inicial
          </label>
          <input
            type="number"
            id="energy"
            value={energyText}
            onChange={(e) => setEnergyText(e.target.value)}
            className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500 sm:text-sm"
            placeholder="10"
          />
        </div>

        {/* Botón enviar */}
        <button
          type="submit"
          disabled={loading}
          className={`inline-flex justify-center px-6 py-3 rounded-md font-semibold text-white transition-colors ${
            loading ? "bg-indigo-300" : "bg-indigo-600 hover:bg-indigo-700"
          } focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500`}
        >
          {loading ? "Procesando..." : "Enviar"}
        </button>
      </form>

      {/* Mostrar error si existe */}
      {error && (
        <div className="mt-6 w-full bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded">
          <strong className="font-bold">Error:</strong>
          <span className="block">{error}</span>
        </div>
      )}

      {/* Mostrar resultado si existe */}
      {result && (
        <div className="mt-6 w-full bg-green-50 border border-green-400 text-green-700 px-4 py-3 rounded">
          <p className="font-semibold">Resultado:</p>
          <p>
            <strong>Ruta óptima:</strong> {JSON.stringify(result.prPath)}
          </p>
          <p>
            <strong>Energía final:</strong> {result.prFinalEnergy}
          </p>
        </div>
      )}
    </section>
  );
}
