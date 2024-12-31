{
  description = "System Setup";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    hyprland.url = "git+https://github.com/hyprwm/Hyprland?submodules=1";

    # Darwin config.
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # Home Manager.
    home-manager =  {
      url = "github:nix-community/home-manager/";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stylix.url = "github:danth/stylix";
    hyprpanel.url = "github:Jas-SinghFSU/HyprPanel";

    ghostty = {
      url = "github:ghostty-org/ghostty";
    };
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager, hyprland, stylix, hyprpanel,  ghostty }:
  let
    linuxSystems = ["x86_64-linux"];
    darwinSystems = ["aarch64-darwin"];
  in
  {
    darwinConfigurations = nixpkgs.lib.genAttrs darwinSystems (system: 
      nix-darwin.lib.darwinSystem {
        inherit system;
        specialArgs = { inherit inputs; };
        modules = [
          home-manager.darwinModules.home-manager
          ./hosts/darwin.nix
        ];
      }
    );

    nixosConfigurations = nixpkgs.lib.genAttrs linuxSystems (system: 
      nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit inputs; };
        modules = [
          home-manager.nixosModules.home-manager
          stylix.nixosModules.stylix
          { nixpkgs.overlays = [inputs.hyprpanel.overlay]; }
          ./hosts/nixos.nix
          {
            environment.systemPackages = [
              ghostty.packages.x86_64-linux.default
            ];
          }
        ];
      }
    );

    homeConfigurations."saul@nixos" = nixpkgs.lib.genAttrs linuxSystems(system: home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            inputs.hyprpanel.overlay
          ];
        };
        extraSpecialArgs = {
          inherit system;
          inherit inputs;
        };
      }
    );
  };
}
