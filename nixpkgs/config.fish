# Only perform vim keybindings when
# the terminal is interactive to
# silence warnings in emacs shell for example

if status is-interactive
  fish_vi_key_bindings
end

set theme_color_scheme gruvbox
set -g theme_nerd_fonts no
set -g theme_powerline_fonts no
set -g theme_display_date no

# asdf ----------------------------
# source ~/.asdf/asdf.fish

# PATHS ---------------------------
set -xg GOPATH $HOME
set -xg ELIXIR_LANG_SERVER_PATH $HOME/Developer/elixir-ls/release
set -xg EDITOR nvim
set -xg NVM_DIR $HOME/.nvm
set -xg PATH $GOPATH/bin $PATH
set -xg PATH $HOME/.local/bin $PATH
set -xg BAT_THEME "Nord"

alias vim="nvim"

# dev -----------------------------
source /opt/dev/dev.fish

# ERL persistent history
set -xg ERL_AFLAGS '-kernel shell_history enabled'
alias sd="/Users/saulecabrera/src/github.com/Shopify/shopify-cli/bin/shopify"
alias jd="/Users/saulecabrera/src/github.com/Shopify/javy/target/release/javy"
alias cl="clear"

# opam configuration
source /Users/saulecabrera/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f /Users/saulecabrera/.ghcup/env ; and set -gx PATH $HOME/.cabal/bin /Users/saulecabrera/.ghcup/bin $PATH

# Wasmer
export WASMER_DIR="/Users/saulecabrera/.wasmer"
[ -s "$WASMER_DIR/wasmer.sh" ] && source "$WASMER_DIR/wasmer.sh"
set -g fish_user_paths "/usr/local/opt/llvm/bin" $fish_user_paths

set -gx WASMTIME_HOME "$HOME/.wasmtime"

string match -r ".wasmtime" "$PATH" > /dev/null; or set -gx PATH "$WASMTIME_HOME/bin" $PATH

starship init fish | source

mcfly init fish | source
