{ lib, pkgs, config, ... }:

{
  imports =
  [
    ../shared/programs.nix
    ../shared/packages.nix
    ./packages.nix
    ./programs.nix
    ./services.nix
  ];

  home = {
    username = "saul";
    homeDirectory = "/home/saul";
    stateVersion = "25.05";
  };

  home.file.".config/zed/keymap.json".text = builtins.readFile ../shared/zed-keymap.json;
  home.file.".config/zed/settings.json".text = builtins.readFile ../shared/zed-settings.json;
  home.file.".config/ghostty/config".text = builtins.readFile ../shared/ghostty-config;

  gtk = {
    enable = true;
    theme = {
      name = "WhiteSur";
      package = pkgs.whitesur-gtk-theme;
    };
    iconTheme = {
      name = "WhiteSur";
      package = pkgs.whitesur-icon-theme;
    };
  };

  programs.home-manager.enable = true;
}
