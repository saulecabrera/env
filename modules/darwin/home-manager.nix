{lib, config, pkgs, inputs, ...}: 

let 
  user = "saulecabrera";
in
{
  users.users.${user} = {
    name = "${user}";
    home = "/Users/${user}";
    isHidden = false;
    shell = pkgs.zsh;
  };

  home-manager = {
    extraSpecialArgs = { inherit inputs; };
    users.${user} = {pkgs, config, lib, ...}: {
      home = {
        stateVersion = "25.05";
        username = user;
        homeDirectory = "/Users/${user}";
        packages = pkgs.callPackage ./packages.nix {};
      };
      programs = {} // import ../shared/programs.nix {inherit config pkgs lib;};
    };
  };
} 
