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
- Press F1 when running a program, you will see a lot of info
##### Debug menu
- `n` -- go to the next instruction
- `up-down` -- go +-2 addresses in memory
- `left-right` -- go +-1 addresses in memory
- `pgUp-pgDown` -- go +-8 addresses in memory
- `b` -- set/unset breakpoint in current cursor position
- `p` -- pause/continue (also continue when at breakpoint)
- `+/-` -- control speed

## Development
Try `nix develop`.
