{config, pkgs, lib, ...}:

{
   gpg = {
    enable = true;
    # https://support.yubico.com/hc/en-us/articles/4819584884124-Resolving-GPG-s-CCID-conflicts
    scdaemonSettings = {
      disable-ccid = true;
    };
    settings = {
      personal-cipher-preferences = "AES256 AES192 AES";
      personal-digest-preferences = "SHA512 SHA384 SHA256";
      personal-compress-preferences = "ZLIB BZIP2 ZIP Uncompressed";
      default-preference-list = "SHA512 SHA384 SHA256 AES256 AES192 AES ZLIB BZIP2 ZIP Uncompressed";
      cert-digest-algo = "SHA512";
      s2k-digest-algo = "SHA512";
      s2k-cipher-algo = "AES256";
      charset = "utf-8";
      fixed-list-mode = true;
      no-comments = true;
      no-emit-version = true;
      keyid-format = "0xlong";
      list-options = "show-uid-validity";
      verify-options = "show-uid-validity";
      with-fingerprint = true;
      require-cross-certification = true;
      no-symkey-cache = true;
      use-agent = true;
      throw-keyids = true;
    };
  };

  rofi = {
    enable = true;
    theme = ./rofi.rasi;
  };

  hyprlock = {
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
          font_family = PragmataPro Mono Liga
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
          font_family = PragmataPro Mono Liga
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
          font_family = PragmataPro Mono Liga Bold
          fade_on_empty = false
          placeholder_text = <span foreground="##fbf1c7">Password</span>
          hide_input = false
          position = 0, 50
          halign = center
          valign = bottom
      }
    '';
  };

  waybar = {
    enable = true;
    style = builtins.readFile ./waybar.css;
    settings = [{
      layer = "top";
      position = "top";

      modules-left =  ["hyprland/workspaces"];
      modules-center =  ["clock"];
      modules-right = [
        "pulseaudio"
        "network"
        "battery"
      ];

    
      battery = {
        format = "<span>{icon}</span> {capacity}%";
        format-icons = [
            " "
            " "
            " "
            " "
            " "
        ];
        format-charging = "<span> </span>{capacity}%";
        format-full = "<span> </span>{capacity}%";
        format-warning = "<span> </span>{capacity}%";
        interval = 5;
        states = {
          warning = 20;
        };
        format-time = "{H}h{M}m";
        tooltip = true;
        tooltip-format = "{time}";
      };

      clock = {
        format =  "{:%I:%M %p}";
        format-alt = "{:%a, %d. %b  %I:%M %p}";
      };

      pulseaudio = {
        format = "{icon} {volume}%";
        format-muted = "<span>🔇 </span> {volume}%";
        format-icons = {
          default = [ "<span> </span>" ];
        };
        scroll-step = 2;
        on-click = "pamixer -t";
      };

      network = {
        format-wifi = "<span>  </span> {signalStrength}%";
        format-ethernet = "<span>󰀂</span>";
        tooltip-format = "Connected to {essid} {ifname} via {gwaddr}";
        format-linked = "{ifname} (No IP)";
        format-disconnected = "<span> 󰖪 </span>";
      };

      "hyprland/workspaces" = {
        disable-scroll = true;
        all-outputs = true;
        format = "{icon}";
        persistent_workspaces = {
          "1" = {};
          "2" = {};
        };
      };
    }];
  };
}
