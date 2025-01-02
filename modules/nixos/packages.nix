{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gnumake
    yubikey-manager
    via
    clang
    zulip
    clangStdenv
  ];
}
