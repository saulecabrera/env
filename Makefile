# Laptop setup
#
.PHONY: clean link

prepare:
	rm -rf ~/.config/nixpkgs/
	rm -rf ~/.config/alacritty/
	rm -rf ~/.config/jrnl
	rm -rf ~/.config/helix
	mkdir ~/.config/nixpkgs/
	mkdir ~/.config/alacritty/
	mkdir ~/.config/jrnl
	mkdir ~/.config/helix

link:
	ln -s ~/Developer/env/nixpkgs/home.nix ~/.config/nixpkgs
	ln -s ~/Developer/env/nixpkgs/config.nix ~/.config/nixpkgs
	ln -s ~/Developer/env/nixpkgs/config.dev.nix ~/.config/nixpkgs
	ln -s ~/Developer/env/alacritty/alacritty.toml ~/.config/alacritty
	ln -s ~/Developer/env/jrnl/jrnl.yaml ~/.config/jrnl/
	ln -s ~/Developer/env/helix/config.toml ~/.config/helix/

