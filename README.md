<div align="center">
  <h1>@saulecabrera's env<h1>
</div>

## Pre-requisites

### macOS

* Install `nix-darwin` https://github.com/LnL7/nix-darwin, pretty much run:
  * Run `nix run nix-darwin --extra-experimental-features "nix-command flakes" -- switch --flake /path/to/this/repo#aarch64-darwin --impure`
* If you make changes and want to rebuild:
  * `darwin-rebuild switch --flake /path/to/this/repo#aarch64-darwin --impure`

### NixOS

TODO
