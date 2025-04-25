{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gnumake
    yubikey-manager
    via
    zulip
    wbg
    gphoto2
    ffmpeg
  ];
}
