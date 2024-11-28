{ config, pkgs, ... }: 

{
  imports = [
    ../modules/darwin/home-manager.nix
  ];

  system = {
    stateVersion = 5;
  };
}
