### ProggerX/chipi-chapa
# Monadic CHIP-8 emulator written in Haskell

Supports only CHIP-8 instructions for now.

## Installation
#### Nix 
If you are lucky nix user, you can use flake in this repo.\
chipi-chapa is also cached on [our cachix](https://balds.cachix.org)

#### No nix?
Use cabal idk

## Usage
- Run chip-8 ROM: `chipi-chapa <path-to-rom>`
#### Debug
- Set `CHIPI_CHAPA_DEBUG` env to anything, you will get additional output in stdout.

## Development
Try `nix develop`.
