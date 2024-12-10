{ pkgs, ... }:

with pkgs; [
   shellharden
   nix-prefetch-git
   fd
   nodejs
   nodePackages.pnpm
   hugo
   ripgrep
   figlet
   yarn
   elixir
   fennel
   fennel-ls
   fnlfmt
   zsh-fzf-tab
   rustup
   binaryen
   jq
   sesh
   zoxide
]


  
