# Changelog

## [Unreleased]

### Added

- _Clear_ button

### Changed

- Use `WorldDimensions` type alias instead of brute `Record`

## [0.3.0] 2022-08-07

### Added

- Simple cell manipulation by the user (still doesn't support dragging)

### Changed

- Increased default ticking rate
- Call `mkWorldGrid` from within `renderWorld` for simplicity
- Use `Cell` data type with `Alive` and `Dead` constructors
- Move `World` creation functions to `Gol.Logic` module

### Fixed

- Mistakenly hard-coding random cell probability

## [0.2.0] 2022-08-04

### Added

- Basic UI with Start/Stop button and tick rate controller

### Changed

- Canvas size now defined in CSS and in Main.purs
- "Gol" component now makes proper idiomatic use of props

### Removed

- ISC license

## [0.1.0] 2022-07-27

### Added

- Basic working Game of Life, no user controls
- Array2D library with relevant functions
