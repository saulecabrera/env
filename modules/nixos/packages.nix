{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gnumake
    yubikey-manager
    via
    zulip
    gphoto2
    ffmpeg
    wbg
    hypridle
    hyprlock
  ];
}
