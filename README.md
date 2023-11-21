# Game of Life in Assembly Language

## Overview
This project is an implementation of Conway's Game of Life in assembly language, specifically designed for the NIOS II processor on the Gecko4Education board. It's an interactive simulation where cells on a grid live or die based on predefined rules.

## Key Features
- Implementation of the Game of Life in assembly language.
- Interactive control using the Gecko4Education board.
- Utilizes LED arrays for game visualization.
- Implements game rules such as underpopulation, overpopulation, reproduction, and stasis.

## Gameplay
- The game is displayed on an LED array, with each pixel representing a cell.
- Players can interact with the game using buttons on the board, choosing different seeds, changing game speed, and pausing or resetting the game.
- The game evolves automatically based on its initial state and player inputs.

## Technical Details
- Game state management and LED control implemented in assembly.
- Utilizes memory-mapped registers for LED control and button inputs.
- Procedures for setting and getting game state, drawing on LEDs, and handling user input.

## Game Image
![Game of Life](https://github.com/brosio-lsn/GameOfLife-Assemby/blob/main/view.png)

## Acknowledgments
- Inspired by John Conway's classic cellular automaton concept.
- Developed as part of an educational initiative at EPFL.


