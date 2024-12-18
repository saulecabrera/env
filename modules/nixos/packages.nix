{ pkgs }:

with pkgs;
let shared-packages = import ../shared/packages.nix { inherit pkgs; }; in
shared-packages ++ [
  gnumake
  yubikey-manager
  via
  clang
  zulip
  clangStdenv
  wbg
  pamixer
]
