
{ pkgs, ... }:

{
  home.packages = with pkgs; [
    raycast
    aerospace
  ];
}
