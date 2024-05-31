# Course Projects for CISC681 Artificial Intelligence
## Introduction

Hello, my name is Ethan Chang and this repository contains 3 course projects for a class on AI. Projects were done in Lisp packages using Portacle.
The projects are described below.
1) PS1 - N-puzzle solver
2) PS2 - Sudoku solver using AC-3 and backtracking search
3) PS3 - Training a neural network to learn Hexapawn

## PS1 - N-puzzle Solver
This project implements a variety of search algorithms to solve the N-puzzle. The algorithms used were Iterative Deepening Search, Breadth-First Search, and A* Search using two heuristics.
The two heuristics were the Number of Misplaced Tiles and the Manhattan Distance.
[Writeup/report](https://docs.google.com/document/d/1ylWqZrsUmWNztl43UPPuS5eRtdTv4bEViVp3a0m2uWU/edit?usp=sharing)

## PS2 - Sudoku Solver Using AC-3 and Backtracking Search
This project implements the AC-3 algorithm to solve sudoku puzzles. AC-3 is one of a series of algorithms used to solve constraint satisfaction problems (CSPs). Additionally, I utilized a backtracking search to recursively try values to solve the problem and backtrack to the nearest guess if it fails.
[Writeup/report](https://docs.google.com/document/d/17gS1S7Uu0jBnVigamzvnrjGK1i_gDGBR0mX9t25E3-0/edit?usp=sharing)

## PS3 - Training a Neural Network to Learn Hexapawn
Hexapawn is a simplified version of the game of chess. The board is a 3x3 matrix with only pawns on the starting rows. The allowed moves for the pawns are similar to that of classical chess, but the pawn cannot move two squares forward for its first move. A player wins if their pawn reaches the other end of the board or if the opponent cannot make a legal move. A fully connected neural network, implemented from scratch in Lisp using adjacency matrices, was trained to learn a simple bit adder as well as learn the optimal moves for any given (valid) board state.
[Writeup/report](https://docs.google.com/document/d/1Tl3tSFhI6noR2rkKK1AyDQV2wGutoaeLrp2czU_cv90/edit?usp=sharing)
