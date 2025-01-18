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

    programs.hyprlock = {
    enable = true;
    extraConfig = ''
      # BACKGROUND
       background {
          monitor =
          path = screenshot
          blur_passes = 2
          contrast = 0.8916
          brightness = 0.8172
          vibrancy = 0.1696
          vibrancy_darkness = 0.0
      }
      # GENERAL
      general {
          hide_cursor = true
          no_fade_in = false
          grace = 0
          disable_loading_bar = false
          ignore_empty_input = true
          fractional_scaling = 0
      }
      # Time
      label {
          monitor = 
          text = cmd[update:1000] echo "$(date +"%k:%M")"
          color = rgba(235, 219, 178, .9)
          font_size = 115
          shadow_passes = 3
          position = 0, -25
          halign = center
          valign = top
      }
      # Day
      label {
          monitor =
          text = cmd[update:1000] echo "- $(date +"%A, %B %d") -"
          color = rgba(235, 219, 178, .9)
          font_size = 18
          shadow_passes = 3
          position = 0, -225
          halign = center
          valign = top
      }
      # INPUT FIELD
      input-field {
          monitor =
          size = 300, 50
          outline_thickness = 0
          rounding = 15
          dots_size = 0.25 # Scale of input-field height, 0.2 - 0.8
          dots_spacing = 0.4 # Scale of dots' absolute size, 0.0 - 1.0
          dots_center = true
          outer_color = rgba(102, 92, 84, .33)
          inner_color = rgba(102, 92, 84, .33)
          color = rgba(235, 219, 178, .9)
          font_color = rgba(235, 219, 178, .9)
          font_size = 15
          fade_on_empty = false
          placeholder_text = <span foreground="##fbf1c7">Password</span>
          hide_input = false
          position = 0, 50
          halign = center
          valign = bottom
      }
    '';
  };

  stylix.targets = {
    neovim.enable = false;
    tmux.enable = false;
    alacritty.enable = false;
    bat.enable = false;
    hyprlock.enable = false;
    rofi.enable = false;
  };
}
