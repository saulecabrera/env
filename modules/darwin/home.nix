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

  home.file.".config/zed/keymap.json".text = builtins.readFile ../shared/zed-keymap.json;
  home.file.".config/zed/settings.json".text = builtins.readFile ../shared/zed-settings.json;

  programs.home-manager.enable = true;
}
