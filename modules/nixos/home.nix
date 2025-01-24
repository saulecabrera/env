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
