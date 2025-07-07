{
  description = "System Setup";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Darwin config.
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # Home Manager.
    home-manager =  {
      url = "github:nix-community/home-manager/";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ghostty = {
      url = "github:ghostty-org/ghostty";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    hyprland.url = "github:hyprwm/Hyprland";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager, ghostty, nixos-hardware, hyprland }:
  let
    linuxSystems = ["x86_64-linux" "aarch64-linux"];  # Added aarch64-linux
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
          (if system == "x86_64-linux" then ./hosts/nixos-x86_64.nix else ./hosts/nixos-aarch64.nix)
          {
            environment.systemPackages = [
              # Adjusted to support both architectures
              (if system == "x86_64-linux" then ghostty.packages.x86_64-linux.default else ghostty.packages.aarch64-linux.default)
            ];
          }
        ];
      }
    );

    homeConfigurations."saul@nixos" = nixpkgs.lib.genAttrs linuxSystems (system: home-manager.lib.homeManagerConfiguration {
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
