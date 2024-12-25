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

  gtk = {
    enable = true;
    iconTheme = {
      name = "Gruvbox-Plus-Dark";
      package = pkgs.gruvbox-plus-icons;
    };
  };

  programs.home-manager.enable = true;

  wayland.windowManager.hyprland = {
    enable = true;
    extraConfig = builtins.readFile ./hyprland.conf;
  };

  stylix.targets = {
    neovim.enable = false;
    rofi.enable = false;
    alacritty.enable = false;
    bat.enable = false;
    hyprlock.enable = false;
  };
}
