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
  home.file.".config/ghostty/config".text = builtins.readFile ../shared/ghostty-config;
  home.file.".aerospace.toml".text = builtins.readFile ./aerospace.toml;

  programs.home-manager.enable = true;
}
