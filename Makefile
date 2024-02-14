# Laptop setup
#
# Commit signing
# - Create a key, following:
#   https://docs.github.com/en/authentication/managing-commit-signature-verification/generating-a-new-gpg-key
# - Install pinentry-mac
# - Create ~/.gnupg/gpg-agent.conf if doesn't exist
# - Add the following to gpg's config:
#     pinentry-program /opt/homebrew/bin/pinentry-mac
#     default-cache-ttl max-cache-ttl
# - Tell git about the GPG key:
#   https://docs.github.com/en/authentication/managing-commit-signature-verification/telling-git-about-your-signing-key
#     git config --global user.signingkey 3AA5C34371567BD2 (example key taken
#     from GitHub's link)

.PHONY: clean link

prepare:
	rm -rf ~/.config/nixpkgs/
	rm -rf ~/.config/zellij/
	rm -rf ~/.config/alacritty/
	mkdir ~/.config/nixpkgs/
	mkdir ~/.config/alacritty/
	mkdir ~/.config/zellij/
	mkdir ~/.config/zellij/layouts

link:
	ln -s ~/Developer/env/nixpkgs/home.nix ~/.config/nixpkgs
	ln -s ~/Developer/env/nixpkgs/config.nix ~/.config/nixpkgs
	ln -s ~/Developer/env/nixpkgs/config.dev.nix ~/.config/nixpkgs
	ln -s ~/Developer/env/alacritty/alacritty.toml ~/.config/alacritty
	ln -s ~/Developer/env/zellij/config.kdl ~/.config/zellij
	ln -s ~/Developer/env/zellij/layouts/main.kdl ~/.config/zellij/layouts/

