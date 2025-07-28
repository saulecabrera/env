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

    gtk3.extraConfig = {
        gtk-xft-antialias = 1;
        gtk-xft-hinting = 1;
        gtk-xft-hintstyle = "hintslight";
      };

      gtk4.extraConfig = {
        gtk-xft-antialias = 1;
        gtk-xft-hinting = 1;
        gtk-xft-hintstyle = "hintslight";
      };
  };

  dconf.settings = {
    "org/gnome/desktop/wm/keybindings" = {
      activate-window-menu = "disabled";
      toggle-message-tray = "disabled";
      close = ["<Super>q"];
      maximize = "disabled";
      minimize = ["<Super>comma"];
      move-to-workspace-1 = ["<Super><Shift>1"];
      switch-to-workspace-1 = ["<Super>1"];
      move-to-workspace-2 = ["<Super><Shift>2"];
      switch-to-workspace-2 = ["<Super>2"];
      move-to-workspace-3 = ["<Super><Shift>3"];
      switch-to-workspace-3 = ["<Super>3"];
      move-to-workspace-4 = ["<Super><Shift>4"];
      switch-to-workspace-4 = ["<Super>4"];
      move-to-workspace-5 = ["<Super><Shift>5"];
      switch-to-workspace-5 = ["<Super>5"];
      unmaximize = "disabled";
    };
    "org/gnome/desktop/wm/preferences" = {
      button-layout = "close,minimize,maximize:appmenu";
      num-workspaces = 10;
    };
  };

  programs.home-manager.enable = true;
}
