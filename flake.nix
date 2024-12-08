{
  description = "System Setup";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

    # Darwin config.
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # Home Manager.
    home-manager =  {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager }:
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
          ./hosts/nixos.nix
        ];
      }
    );
  };
}
