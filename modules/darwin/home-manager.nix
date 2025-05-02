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
    users.saulecabrera.imports = [./home.nix];
  };
  services.emacs.enable = true;
} 
