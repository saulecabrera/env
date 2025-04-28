.PHONY: darwin

darwin:
	sudo darwin-rebuild switch --flake ./#aarch64-darwin --impure
