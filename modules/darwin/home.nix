{ lib, pkgs, config, ... }:

{
  imports = 
  [
    ../shared/programs.nix
    ../shared/packages.nix
    ./packages.nix
  ];

  home = {
    username = "saulecabrera";
    homeDirectory = "/Users/saulecabrera";
    stateVersion = "25.05";
  };

  programs.home-manager.enable = true;
}
