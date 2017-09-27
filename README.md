# Pacman

[![Build Status](https://travis-ci.org/zcesur/pacman.svg?branch=master)](https://travis-ci.org/zcesur/pacman)

Haskell implementation of the Pacman game. It is rendered using Gloss, which is a high-level wrapper for OpenGL.

Use WASD to move around. Eat all Pac-Dots to beat the game. Try to not run into ghosts. Eat Power Pellets to scare the ghosts away and eat them.

A short GIF showing the gameplay will be added soon.

## Installation on Ubuntu

### Prerequisites

`freeglut3` is a system library that is needed to install `GLUT`, which is a "Haskell binding for the OpenGL Utility Toolkit, a window system independent toolkit for writing OpenGL programs."

```{bash}
sudo apt-get install freeglut3
```

### The game

The package might be on Hackage some day, but for now, you'll need Git to install it.

```{bash}
git clone https://github.com/zcesur/pacman.git
cd pacman
stack install
stack exec pacman
```

## Contributing

Contributions are welcome. You can visit [this](https://github.com/zcesur/pacman/projects/1) page to see the features that I am planning to implement in the near future.

## License

This project is released under the terms of the MIT license. Please see [LICENSE](./LICENSE) for more information.
