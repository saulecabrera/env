.PHONY: darwin

darwin:
	sudo darwin-rebuild switch --flake ./#aarch64-darwin --impure

nixos:
	sudo nixos-rebuild switch --flake .#x86_64-linux --impure
