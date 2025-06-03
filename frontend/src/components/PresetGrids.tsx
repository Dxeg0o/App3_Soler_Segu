"use client";

import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { Play } from "lucide-react";

interface PresetGridsProps {
  onLoadPreset: (grid: number[][], energy: number) => void;
}

const presets = [
  {
    name: "Beginner Forest",
    description: "A simple 3×3 grid perfect for learning",
    grid: [
      [1, -1, 2],
      [0, 3, -2],
      [2, 1, 4],
    ],
    energy: 8,
    difficulty: "Easy",
  },
  {
    name: "Enchanted Path",
    description: "Medium difficulty with mixed energy zones",
    grid: [
      [2, -1, 0, 3],
      [1, 4, -3, 2],
      [0, 2, 1, -4],
      [3, -2, 2, 1],
    ],
    energy: 12,
    difficulty: "Medium",
  },
  {
    name: "Dragon's Lair",
    description: "Challenging 5×5 grid with high stakes",
    grid: [
      [3, -2, 1, -4, 2],
      [-1, 5, -3, 1, -2],
      [2, -1, 4, -5, 3],
      [-3, 2, -1, 6, -2],
      [1, -4, 2, -1, 5],
    ],
    energy: 15,
    difficulty: "Hard",
  },
  {
    name: "Mystic Maze",
    description: "Complex 6×6 grid for experts",
    grid: [
      [2, -3, 1, 4, -2, 3],
      [-1, 5, -4, 2, 1, -3],
      [3, -2, 6, -1, 4, -5],
      [-4, 1, -3, 5, -2, 2],
      [2, -5, 3, -1, 6, -4],
      [-2, 4, -3, 2, -1, 7],
    ],
    energy: 20,
    difficulty: "Expert",
  },
];

export default function PresetGrids({ onLoadPreset }: PresetGridsProps) {
  const getDifficultyColor = (difficulty: string) => {
    switch (difficulty) {
      case "Easy":
        return "bg-green-100 text-green-800";
      case "Medium":
        return "bg-yellow-100 text-yellow-800";
      case "Hard":
        return "bg-orange-100 text-orange-800";
      case "Expert":
        return "bg-red-100 text-red-800";
      default:
        return "bg-gray-100 text-gray-800";
    }
  };

  return (
    <div className="space-y-4">
      <div className="text-center mb-6">
        <h3 className="text-lg font-semibold text-gray-800 mb-2">
          Choose a Preset Grid
        </h3>
        <p className="text-sm text-gray-600">
          Select a pre-designed forest to get started quickly, or use them as
          inspiration for your own grids.
        </p>
      </div>

      <div className="grid gap-4">
        {presets.map((preset, index) => (
          <Card key={index} className="hover:shadow-md transition-shadow">
            <CardHeader className="pb-3">
              <div className="flex items-center justify-between">
                <CardTitle className="text-lg">{preset.name}</CardTitle>
                <Badge className={getDifficultyColor(preset.difficulty)}>
                  {preset.difficulty}
                </Badge>
              </div>
              <p className="text-sm text-gray-600">{preset.description}</p>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="flex items-center gap-4 text-sm text-gray-600">
                <span>
                  Grid Size:{" "}
                  <strong>
                    {preset.grid.length}×{preset.grid[0].length}
                  </strong>
                </span>
                <span>
                  Starting Energy: <strong>{preset.energy}</strong>
                </span>
              </div>

              {/* Mini Grid Preview */}
              <div className="flex justify-center">
                <div
                  className="grid gap-1 p-2 bg-gray-50 rounded border"
                  style={{
                    gridTemplateColumns: `repeat(${preset.grid[0].length}, minmax(0, 1fr))`,
                  }}
                >
                  {preset.grid.map((row, rowIndex) =>
                    row.map((cell, colIndex) => (
                      <div
                        key={`${rowIndex}-${colIndex}`}
                        className={`w-6 h-6 text-xs flex items-center justify-center rounded font-semibold ${
                          cell > 0
                            ? "bg-green-100 text-green-800"
                            : cell < 0
                            ? "bg-red-100 text-red-800"
                            : "bg-gray-100 text-gray-800"
                        }`}
                      >
                        {cell}
                      </div>
                    ))
                  )}
                </div>
              </div>

              <Button
                onClick={() => onLoadPreset(preset.grid, preset.energy)}
                className="w-full"
                variant="outline"
              >
                <Play className="w-4 h-4 mr-2" />
                Load This Grid
              </Button>
            </CardContent>
          </Card>
        ))}
      </div>
    </div>
  );
}
