"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Plus, Minus } from "lucide-react";

interface GridEditorProps {
  grid: number[][];
  onGridChange: (newGrid: number[][]) => void;
  path?: [number, number][];
}

export default function GridEditor({
  grid,
  onGridChange,
  path,
}: GridEditorProps) {
  const [editingCell, setEditingCell] = useState<[number, number] | null>(null);

  const updateCell = (row: number, col: number, value: number) => {
    const newGrid = grid.map((r, rIndex) =>
      r.map((c, cIndex) => (rIndex === row && cIndex === col ? value : c))
    );
    onGridChange(newGrid);
  };

  /**
   * Increase grid size by one row and one column to keep it square
   */
  const increaseSize = () => {
    const newSize = grid.length + 1;
    // add a column to each existing row
    const expanded = grid.map((row) => [...row, 0]);
    // add the new row at the bottom
    const newRow = new Array(newSize).fill(0);
    onGridChange([...expanded, newRow]);
  };

  /**
   * Decrease grid size by one row and one column to keep it square
   */
  const decreaseSize = () => {
    if (grid.length > 2) {
      const trimmedRows = grid.map((row) => row.slice(0, -1)).slice(0, -1);
      onGridChange(trimmedRows);
    }
  };

  const isInPath = (row: number, col: number): boolean => {
    return path?.some(([r, c]) => r === row && c === col) || false;
  };

  const getPathIndex = (row: number, col: number): number => {
    return path?.findIndex(([r, c]) => r === row && c === col) ?? -1;
  };

  const getCellColor = (value: number, row: number, col: number) => {
    const inPath = isInPath(row, col);
    const pathIndex = getPathIndex(row, col);

    if (inPath) {
      if (pathIndex === 0) return "bg-green-500 text-white border-green-600"; // Start
      if (pathIndex === (path?.length ?? 0) - 1)
        return "bg-red-500 text-white border-red-600"; // End
      return "bg-blue-500 text-white border-blue-600"; // Path
    }

    if (value > 0)
      return "bg-green-100 text-green-800 border-green-300 hover:bg-green-200";
    if (value < 0)
      return "bg-red-100 text-red-800 border-red-300 hover:bg-red-200";
    return "bg-gray-100 text-gray-800 border-gray-300 hover:bg-gray-200";
  };

  return (
    <div className="space-y-4">
      {/* Grid Controls */}
      <div className="flex flex-wrap gap-2">
        <Button onClick={increaseSize} size="sm" variant="outline">
          <Plus className="w-4 h-4 mr-1" />
          Aumentar Tamaño
        </Button>
        <Button
          onClick={decreaseSize}
          size="sm"
          variant="outline"
          disabled={grid.length <= 2}
        >
          <Minus className="w-4 h-4 mr-1" />
          Reducir Tamaño
        </Button>
      </div>

      {/* Grid */}
      <div className="inline-block border-2 border-gray-300 rounded-lg p-2 bg-white">
        <div
          className="grid gap-1"
          style={{
            gridTemplateColumns: `repeat(${
              grid[0]?.length || 0
            }, minmax(0, 1fr))`,
          }}
        >
          {grid.map((row, rowIndex) =>
            row.map((cell, colIndex) => {
              const inPath = isInPath(rowIndex, colIndex);
              const pathIndex = getPathIndex(rowIndex, colIndex);

              return (
                <div key={`${rowIndex}-${colIndex}`} className="relative">
                  <button
                    className={`w-12 h-12 border-2 rounded-md font-semibold text-sm transition-all duration-200 ${getCellColor(
                      cell,
                      rowIndex,
                      colIndex
                    )}`}
                    onClick={() => setEditingCell([rowIndex, colIndex])}
                  >
                    {cell}
                  </button>
                  {inPath && (
                    <div className="absolute -top-1 -right-1 w-5 h-5 bg-yellow-400 text-yellow-900 rounded-full text-xs flex items-center justify-center font-bold">
                      {pathIndex + 1}
                    </div>
                  )}
                </div>
              );
            })
          )}
        </div>
      </div>

      {/* Cell Editor */}
      {editingCell && (
        <div className="flex items-center gap-2 p-3 bg-gray-50 rounded-lg">
          <span className="text-sm">
            Editar celda ({editingCell[0]}, {editingCell[1]}):
          </span>
          <input
            type="number"
            value={grid[editingCell[0]][editingCell[1]]}
            onChange={(e) =>
              updateCell(
                editingCell[0],
                editingCell[1],
                Number.parseInt(e.target.value) || 0
              )
            }
            className="w-20 px-2 py-1 border border-gray-300 rounded text-sm"
            autoFocus
          />
          <Button size="sm" onClick={() => setEditingCell(null)}>
            Hecho
          </Button>
        </div>
      )}

      {/* Legend */}
      <div className="flex flex-wrap gap-4 text-xs text-gray-600">
        <div className="flex items-center gap-1">
          <div className="w-3 h-3 bg-green-100 border border-green-300 rounded"></div>
          <span>Positivo (Aumenta Energía)</span>
        </div>
        <div className="flex items-center gap-1">
          <div className="w-3 h-3 bg-red-100 border border-red-300 rounded"></div>
          <span>Negativo (Reduce Energía)</span>
        </div>
        <div className="flex items-center gap-1">
          <div className="w-3 h-3 bg-gray-100 border border-gray-300 rounded"></div>
          <span>Neutro</span>
        </div>
        {path && (
          <>
            <div className="flex items-center gap-1">
              <div className="w-3 h-3 bg-green-500 rounded"></div>
              <span>Inicio</span>
            </div>
            <div className="flex items-center gap-1">
              <div className="w-3 h-3 bg-blue-500 rounded"></div>
              <span>Camino</span>
            </div>
            <div className="flex items-center gap-1">
              <div className="w-3 h-3 bg-red-500 rounded"></div>
              <span>Fin</span>
            </div>
          </>
        )}
      </div>
    </div>
  );
}
