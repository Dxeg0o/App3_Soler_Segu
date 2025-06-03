import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { Zap, Target, Plus, Minus, ArrowRight } from "lucide-react";

export default function GameInstructions() {
  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Target className="w-5 h-5 text-purple-600" />
            Objetivo del Juego
          </CardTitle>
        </CardHeader>
        <CardContent>
          <p className="text-gray-700">
            Recorre el bosque mágico desde la esquina superior izquierda hasta
            la inferior derecha buscando la ruta que maximice tu energía. Cada
            loseta de runa aumentará o reducirá tu poder mágico.
          </p>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Zap className="w-5 h-5 text-yellow-500" />
            Cómo Funciona la Energía
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid gap-3">
            <div className="flex items-center gap-3">
              <Badge className="bg-green-100 text-green-800 w-8 h-8 rounded-full flex items-center justify-center">
                <Plus className="w-4 h-4" />
              </Badge>
              <div>
                <strong>Números positivos:</strong> Aumentan tu energía (p.ej.,
                +3 suma 3 puntos de energía)
              </div>
            </div>
            <div className="flex items-center gap-3">
              <Badge className="bg-red-100 text-red-800 w-8 h-8 rounded-full flex items-center justify-center">
                <Minus className="w-4 h-4" />
              </Badge>
              <div>
                <strong>Números negativos:</strong> Reducen tu energía (p.ej., -2
                resta 2 puntos)
              </div>
            </div>
            <div className="flex items-center gap-3">
              <Badge className="bg-gray-100 text-gray-800 w-8 h-8 rounded-full flex items-center justify-center">
                0
              </Badge>
              <div>
                <strong>Cero:</strong> No afecta tu energía
              </div>
            </div>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Cómo Jugar</CardTitle>
        </CardHeader>
        <CardContent>
          <ol className="space-y-3 text-gray-700">
            <li className="flex items-start gap-3">
              <Badge className="bg-purple-100 text-purple-800 w-6 h-6 rounded-full flex items-center justify-center text-sm">
                1
              </Badge>
              <div>
                <strong>Establece la energía inicial:</strong> Elige cuánta
                energía tendrás al comenzar (recomendado: 10-20)
              </div>
            </li>
            <li className="flex items-start gap-3">
              <Badge className="bg-purple-100 text-purple-800 w-6 h-6 rounded-full flex items-center justify-center text-sm">
                2
              </Badge>
              <div>
                <strong>Edita la cuadrícula:</strong> Haz clic en cualquier
                casilla para cambiar su valor de energía o usa los modelos
                predefinidos
              </div>
            </li>
            <li className="flex items-start gap-3">
              <Badge className="bg-purple-100 text-purple-800 w-6 h-6 rounded-full flex items-center justify-center text-sm">
                3
              </Badge>
              <div>
                <strong>Encuentra la ruta:</strong> Haz clic en
                &quot;Encontrar Ruta Óptima&quot; para calcular el mejor
                recorrido
              </div>
            </li>
            <li className="flex items-start gap-3">
              <Badge className="bg-purple-100 text-purple-800 w-6 h-6 rounded-full flex items-center justify-center text-sm">
                4
              </Badge>
              <div>
                <strong>Ver resultados:</strong> La ruta óptima se
                resaltará en la cuadrícula con pasos numerados
              </div>
            </li>
          </ol>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Consejos para el Éxito</CardTitle>
        </CardHeader>
        <CardContent>
          <ul className="space-y-2 text-gray-700">
            <li className="flex items-center gap-2">
              <ArrowRight className="w-4 h-4 text-purple-600" />
              Comienza con las cuadrículas predefinidas para aprender la mecánica
            </li>
            <li className="flex items-center gap-2">
              <ArrowRight className="w-4 h-4 text-purple-600" />
              Una energía inicial mayor te da más flexibilidad
            </li>
            <li className="flex items-center gap-2">
              <ArrowRight className="w-4 h-4 text-purple-600" />
              Prueba a crear cuadrículas con distintos patrones para ver cómo
              cambian las rutas
            </li>
            <li className="flex items-center gap-2">
              <ArrowRight className="w-4 h-4 text-purple-600" />
              El algoritmo encuentra automáticamente la ruta óptima
            </li>
          </ul>
        </CardContent>
      </Card>
    </div>
  );
}
