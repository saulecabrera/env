{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gnumake
    yubikey-manager
    via
    zulip
    llvmPackages.clangUseLLVM
    clangStdenv
    wbg
  ];
}
