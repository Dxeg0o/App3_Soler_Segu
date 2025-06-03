// src/types/api.ts

export type PathResponse = {
  prPath: number[][];
  prFinalEnergy: number;
};

export type ErrorResponse = {
  errMessage: string;
};

export type PathRequest = {
  prGrid: number[][];
  prInitialEnergy: number;
};
