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
            Game Objective
          </CardTitle>
        </CardHeader>
        <CardContent>
          <p className="text-gray-700">
            Navigate through the magical forest from the top-left corner to the
            bottom-right corner, finding the path that maximizes your energy.
            Each rune tile will either boost or drain your magical power!
          </p>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Zap className="w-5 h-5 text-yellow-500" />
            How Energy Works
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid gap-3">
            <div className="flex items-center gap-3">
              <Badge className="bg-green-100 text-green-800 w-8 h-8 rounded-full flex items-center justify-center">
                <Plus className="w-4 h-4" />
              </Badge>
              <div>
                <strong>Positive Numbers:</strong> Boost your energy (e.g., +3
                adds 3 energy points)
              </div>
            </div>
            <div className="flex items-center gap-3">
              <Badge className="bg-red-100 text-red-800 w-8 h-8 rounded-full flex items-center justify-center">
                <Minus className="w-4 h-4" />
              </Badge>
              <div>
                <strong>Negative Numbers:</strong> Drain your energy (e.g., -2
                removes 2 energy points)
              </div>
            </div>
            <div className="flex items-center gap-3">
              <Badge className="bg-gray-100 text-gray-800 w-8 h-8 rounded-full flex items-center justify-center">
                0
              </Badge>
              <div>
                <strong>Zero:</strong> No effect on your energy
              </div>
            </div>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>How to Play</CardTitle>
        </CardHeader>
        <CardContent>
          <ol className="space-y-3 text-gray-700">
            <li className="flex items-start gap-3">
              <Badge className="bg-purple-100 text-purple-800 w-6 h-6 rounded-full flex items-center justify-center text-sm">
                1
              </Badge>
              <div>
                <strong>Set Starting Energy:</strong> Choose how much energy you
                begin with (recommended: 10-20)
              </div>
            </li>
            <li className="flex items-start gap-3">
              <Badge className="bg-purple-100 text-purple-800 w-6 h-6 rounded-full flex items-center justify-center text-sm">
                2
              </Badge>
              <div>
                <strong>Edit the Grid:</strong> Click on any tile to change its
                energy value, or use presets to get started
              </div>
            </li>
            <li className="flex items-start gap-3">
              <Badge className="bg-purple-100 text-purple-800 w-6 h-6 rounded-full flex items-center justify-center text-sm">
                3
              </Badge>
              <div>
                <strong>Find the Path:</strong> Click "Find Optimal Path" to
                calculate the best route
              </div>
            </li>
            <li className="flex items-start gap-3">
              <Badge className="bg-purple-100 text-purple-800 w-6 h-6 rounded-full flex items-center justify-center text-sm">
                4
              </Badge>
              <div>
                <strong>View Results:</strong> The optimal path will be
                highlighted on the grid with numbered steps
              </div>
            </li>
          </ol>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Tips for Success</CardTitle>
        </CardHeader>
        <CardContent>
          <ul className="space-y-2 text-gray-700">
            <li className="flex items-center gap-2">
              <ArrowRight className="w-4 h-4 text-purple-600" />
              Start with preset grids to learn the game mechanics
            </li>
            <li className="flex items-center gap-2">
              <ArrowRight className="w-4 h-4 text-purple-600" />
              Higher starting energy gives you more flexibility
            </li>
            <li className="flex items-center gap-2">
              <ArrowRight className="w-4 h-4 text-purple-600" />
              Try creating grids with different patterns to see how paths change
            </li>
            <li className="flex items-center gap-2">
              <ArrowRight className="w-4 h-4 text-purple-600" />
              The algorithm finds the mathematically optimal path automatically
            </li>
          </ul>
        </CardContent>
      </Card>
    </div>
  );
}
