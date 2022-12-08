# Changelog

## [0.6.0] 2022-12-08

### Added

- Modify the grid by dragging over it

## [0.5.2] 2022-09-06

### Changed

- Fixed Parcel HMR
- Minify bundle

## [0.5.1] 2022-08-25

### Changed

- Restructure development and distribution directories and add corresponding scripts

## [0.5.0] 2022-08-12

### Added

- Header

### Changed

- `World` now wraps around the edges (i.e. it is a [Torus](https://conwaylife.com/wiki/Torus))
- Type definitions moved to separate modules
- Use appropriate HTML semantic structure (`header`, `main`)

## [0.4.0] 2022-08-08

### Added

- _Clear_ and _random_ buttons
- Label and min/max values in tick rate slider

### Changed

- Use `WorldDimensions` type alias instead of brute `Record`
- Separate UI code into `Gol.UI` module

### Fixed

- Margins being applied to all #ui children

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
