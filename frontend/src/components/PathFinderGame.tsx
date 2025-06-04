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
      const baseUrl =
        process.env.NEXT_PUBLIC_API_BASE_URL ?? "http://localhost:8000";
      const resp = await fetch(`${baseUrl}/api/findPath`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          prGrid: grid,
          prInitialEnergy: initialEnergy,
        }),
      });

      if (!resp.ok) {
        throw new Error(`HTTP error: ${resp.status}`);
      }

      const json = await resp.json();

      if ("Right" in json) {
        setResult(json.Right as PathResponse);
      } else if ("Left" in json) {
        setError(json.Left.errMessage as string);
      } else if ("errMessage" in json) {
        setError(json.errMessage as string);
      } else {
        setResult(json as PathResponse);
      }

      setShowPath(true);
    } catch {
      setError("No se pudo encontrar la ruta óptima. Inténtalo de nuevo.");
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
            Bosque de Runas Mágicas
          </h1>
          <Sparkles className="w-8 h-8 text-purple-600" />
        </div>
        <p className="text-lg text-gray-600 max-w-2xl mx-auto">
          Navega por el bosque encantado para encontrar el camino que maximice
          tu energía mágica. Cada loseta de runa aumentará o drenará tu poder.
        </p>
      </div>

      <div className="grid lg:grid-cols-3 gap-6">
        {/* Game Controls */}
        <div className="lg:col-span-2 space-y-6">
          <Tabs defaultValue="play" className="w-full">
            <TabsList className="grid w-full grid-cols-3">
              <TabsTrigger value="play" className="flex items-center gap-2">
                <Play className="w-4 h-4" />
                Jugar
              </TabsTrigger>
              <TabsTrigger value="presets" className="flex items-center gap-2">
                <Target className="w-4 h-4" />
                Modelos
              </TabsTrigger>
              <TabsTrigger value="help" className="flex items-center gap-2">
                <Lightbulb className="w-4 h-4" />
                Cómo Jugar
              </TabsTrigger>
            </TabsList>

            <TabsContent value="play" className="space-y-6">
              {/* Energy Input */}
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <Zap className="w-5 h-5 text-yellow-500" />
                    Energía Inicial
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
                      Puntos de energía para comenzar tu aventura
                    </span>
                  </div>
                </CardContent>
              </Card>

              {/* Grid Editor */}
              <Card>
                <CardHeader>
                  <CardTitle>Cuadrícula del Bosque</CardTitle>
                  <p className="text-sm text-gray-600">
                    Haz clic en las casillas para editar sus valores de energía.
                    Los números positivos aumentan la energía y los negativos la
                    reducen.
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
                      Buscando Ruta...
                    </>
                  ) : (
                    <>
                      <Target className="w-4 h-4 mr-2" />
                      Encontrar Ruta Óptima
                    </>
                  )}
                </Button>
                <Button onClick={handleReset} variant="outline" size="lg">
                  <RotateCcw className="w-4 h-4 mr-2" />
                  Reiniciar
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
                Estado del Juego
              </CardTitle>
            </CardHeader>
            <CardContent className="space-y-3">
              <div className="flex justify-between items-center">
                <span className="text-sm text-gray-600">
                  Tamaño de la Cuadrícula:
                </span>
                <Badge variant="secondary">
                  {grid.length}×{grid[0]?.length || 0}
                </Badge>
              </div>
              <div className="flex justify-between items-center">
                <span className="text-sm text-gray-600">Energía Inicial:</span>
                <Badge className="bg-yellow-100 text-yellow-800">
                  {initialEnergy}
                </Badge>
              </div>
              <div className="flex justify-between items-center">
                <span className="text-sm text-gray-600">Estado:</span>
                <Badge variant={result ? "default" : "secondary"}>
                  {loading
                    ? "Calculando..."
                    : result
                    ? "\u00a1Ruta Encontrada!"
                    : "Listo"}
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
                  ¡Ruta Óptima Encontrada!
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="space-y-2">
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-green-700">
                      Longitud de la Ruta:
                    </span>
                    <Badge className="bg-green-100 text-green-800">
                      {result.prPath.length} pasos
                    </Badge>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-green-700">
                      Energía Final:
                    </span>
                    <Badge className="bg-green-100 text-green-800">
                      {result.prFinalEnergy} puntos
                    </Badge>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-green-700">
                      Energía Obtenida:
                    </span>
                    <Badge className="bg-green-100 text-green-800">
                      +{result.prFinalEnergy - initialEnergy}
                    </Badge>
                  </div>
                </div>

                <div className="pt-2 border-t border-green-200">
                  <p className="text-xs text-green-600 mb-2">
                    Coordenadas de la ruta:
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
