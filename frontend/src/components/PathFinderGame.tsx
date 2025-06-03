"use client";

import { useState } from "react";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import {
  Play,
  RotateCcw,
  Lightbulb,
  Zap,
  Target,
  ArrowRight,
  Sparkles,
} from "lucide-react";
import GridEditor from "@/components/GridEditor";
import GameInstructions from "@/components/GameInstructions";
import PresetGrids from "@/components/PresetGrids";

interface PathResponse {
  prPath: [number, number][];
  prFinalEnergy: number;
}

export default function PathFinderGame() {
  const [grid, setGrid] = useState<number[][]>([
    [2, -1, 0, 3],
    [1, 4, -3, 2],
    [0, 2, 1, -4],
    [3, -2, 2, 1],
  ]);
  const [initialEnergy, setInitialEnergy] = useState<number>(10);
  const [loading, setLoading] = useState<boolean>(false);
  const [result, setResult] = useState<PathResponse | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [showPath, setShowPath] = useState<boolean>(false);

  const handleFindPath = async () => {
    setError(null);
    setResult(null);
    setShowPath(false);
    setLoading(true);

    try {
      // Simulate API call for demo purposes
      await new Promise((resolve) => setTimeout(resolve, 1500));

      // Mock result - in real app, this would call your API
      const mockResult: PathResponse = {
        prPath: [
          [0, 0],
          [1, 1],
          [2, 2],
          [3, 3],
        ],
        prFinalEnergy: initialEnergy + 8,
      };

      setResult(mockResult);
      setShowPath(true);
    } catch {
      setError("Failed to find optimal path. Please try again.");
    } finally {
      setLoading(false);
    }
  };

  const handleReset = () => {
    setResult(null);
    setError(null);
    setShowPath(false);
  };

  const handleLoadPreset = (presetGrid: number[][], presetEnergy: number) => {
    setGrid(presetGrid);
    setInitialEnergy(presetEnergy);
    handleReset();
  };

  return (
    <div className="container mx-auto px-4 py-8 max-w-6xl">
      {/* Header */}
      <div className="text-center mb-8">
        <div className="flex items-center justify-center gap-2 mb-4">
          <Sparkles className="w-8 h-8 text-purple-600" />
          <h1 className="text-4xl font-bold bg-gradient-to-r from-purple-600 to-blue-600 bg-clip-text text-transparent">
            Magical Runes Forest
          </h1>
          <Sparkles className="w-8 h-8 text-purple-600" />
        </div>
        <p className="text-lg text-gray-600 max-w-2xl mx-auto">
          Navigate through the enchanted forest to find the path that maximizes
          your magical energy. Each rune tile will either boost or drain your
          power!
        </p>
      </div>

      <div className="grid lg:grid-cols-3 gap-6">
        {/* Game Controls */}
        <div className="lg:col-span-2 space-y-6">
          <Tabs defaultValue="play" className="w-full">
            <TabsList className="grid w-full grid-cols-3">
              <TabsTrigger value="play" className="flex items-center gap-2">
                <Play className="w-4 h-4" />
                Play
              </TabsTrigger>
              <TabsTrigger value="presets" className="flex items-center gap-2">
                <Target className="w-4 h-4" />
                Presets
              </TabsTrigger>
              <TabsTrigger value="help" className="flex items-center gap-2">
                <Lightbulb className="w-4 h-4" />
                How to Play
              </TabsTrigger>
            </TabsList>

            <TabsContent value="play" className="space-y-6">
              {/* Energy Input */}
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <Zap className="w-5 h-5 text-yellow-500" />
                    Starting Energy
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="flex items-center gap-4">
                    <input
                      type="number"
                      value={initialEnergy}
                      onChange={(e) =>
                        setInitialEnergy(Number.parseInt(e.target.value) || 0)
                      }
                      className="w-24 px-3 py-2 border border-gray-300 rounded-md focus:ring-2 focus:ring-purple-500 focus:border-transparent"
                      min="1"
                      max="100"
                    />
                    <span className="text-sm text-gray-600">
                      Energy points to start your journey
                    </span>
                  </div>
                </CardContent>
              </Card>

              {/* Grid Editor */}
              <Card>
                <CardHeader>
                  <CardTitle>Forest Grid</CardTitle>
                  <p className="text-sm text-gray-600">
                    Click on tiles to edit their energy values. Positive numbers
                    boost energy, negative numbers drain it.
                  </p>
                </CardHeader>
                <CardContent>
                  <GridEditor
                    grid={grid}
                    onGridChange={setGrid}
                    path={showPath ? result?.prPath : undefined}
                  />
                </CardContent>
              </Card>

              {/* Action Buttons */}
              <div className="flex gap-3">
                <Button
                  onClick={handleFindPath}
                  disabled={loading}
                  className="flex-1 bg-gradient-to-r from-purple-600 to-blue-600 hover:from-purple-700 hover:to-blue-700"
                  size="lg"
                >
                  {loading ? (
                    <>
                      <div className="animate-spin rounded-full h-4 w-4 border-2 border-white border-t-transparent mr-2" />
                      Finding Path...
                    </>
                  ) : (
                    <>
                      <Target className="w-4 h-4 mr-2" />
                      Find Optimal Path
                    </>
                  )}
                </Button>
                <Button onClick={handleReset} variant="outline" size="lg">
                  <RotateCcw className="w-4 h-4 mr-2" />
                  Reset
                </Button>
              </div>
            </TabsContent>

            <TabsContent value="presets">
              <PresetGrids onLoadPreset={handleLoadPreset} />
            </TabsContent>

            <TabsContent value="help">
              <GameInstructions />
            </TabsContent>
          </Tabs>
        </div>

        {/* Results Panel */}
        <div className="space-y-6">
          {/* Current Status */}
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Zap className="w-5 h-5 text-yellow-500" />
                Game Status
              </CardTitle>
            </CardHeader>
            <CardContent className="space-y-3">
              <div className="flex justify-between items-center">
                <span className="text-sm text-gray-600">Grid Size:</span>
                <Badge variant="secondary">
                  {grid.length}×{grid[0]?.length || 0}
                </Badge>
              </div>
              <div className="flex justify-between items-center">
                <span className="text-sm text-gray-600">Starting Energy:</span>
                <Badge className="bg-yellow-100 text-yellow-800">
                  {initialEnergy}
                </Badge>
              </div>
              <div className="flex justify-between items-center">
                <span className="text-sm text-gray-600">Status:</span>
                <Badge variant={result ? "default" : "secondary"}>
                  {loading
                    ? "Calculating..."
                    : result
                    ? "Path Found!"
                    : "Ready"}
                </Badge>
              </div>
            </CardContent>
          </Card>

          {/* Results */}
          {error && (
            <Card className="border-red-200 bg-red-50">
              <CardContent className="pt-6">
                <div className="text-red-700">
                  <strong>Error:</strong> {error}
                </div>
              </CardContent>
            </Card>
          )}

          {result && (
            <Card className="border-green-200 bg-green-50">
              <CardHeader>
                <CardTitle className="text-green-800 flex items-center gap-2">
                  <Target className="w-5 h-5" />
                  Optimal Path Found!
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="space-y-2">
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-green-700">Path Length:</span>
                    <Badge className="bg-green-100 text-green-800">
                      {result.prPath.length} steps
                    </Badge>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-green-700">
                      Final Energy:
                    </span>
                    <Badge className="bg-green-100 text-green-800">
                      {result.prFinalEnergy} points
                    </Badge>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-green-700">
                      Energy Gained:
                    </span>
                    <Badge className="bg-green-100 text-green-800">
                      +{result.prFinalEnergy - initialEnergy}
                    </Badge>
                  </div>
                </div>

                <div className="pt-2 border-t border-green-200">
                  <p className="text-xs text-green-600 mb-2">
                    Path coordinates:
                  </p>
                  <div className="flex flex-wrap gap-1">
                    {result.prPath.map((coord, index) => (
                      <span key={index} className="inline-flex items-center">
                        <Badge variant="outline" className="text-xs">
                          ({coord[0]},{coord[1]})
                        </Badge>
                        {index < result.prPath.length - 1 && (
                          <ArrowRight className="w-3 h-3 mx-1 text-green-600" />
                        )}
                      </span>
                    ))}
                  </div>
                </div>
              </CardContent>
            </Card>
          )}
        </div>
      </div>
    </div>
  );
}
