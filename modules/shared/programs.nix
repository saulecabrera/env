{config, pkgs, lib, ...}:

let
  # Compile Fennel to Lua
  luaCfg = pkgs.runCommandLocal "init.lua" {} ''
    ${pkgs.fennel}/bin/fennel --compile ${../shared/init.fnl} > $out
  '';
in 
{
 starship = {
    enable = true;
    enableZshIntegration = true;
    settings = builtins.fromTOML (builtins.readFile ../shared/starship.toml);
  };

  tmux = {
    enable = true;
    extraConfig = builtins.readFile ./tmux.conf;
    plugins = with pkgs.tmuxPlugins; [
      fuzzback
      vim-tmux-navigator
    ];
  };


  fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  direnv = {
   enable = true;
   nix-direnv = {
     enable = true;
   };
  };

  eza = {
    enable = true;
  };

  bat = {
    enable = true;
    config = {
      style = "plain";
      italic-text = "always";
      theme = "ansi";
      pager = "less -XFr";
    };
  };

  git = {
    enable = true;
    userName = "Saúl Cabrera";
    userEmail = "saulecabrera@gmail.com";
    extraConfig = {
      merge = {
        tool = "vimdiff";
        conflictstyle = "diff3";
      };
      diff = {
        tool = "vimdiff";
      };
      core.editor = "nvim";
      commit.gpgsign = true;
      alias = {
        l = "log --pretty=oneline -n 20 --graph --abbrev-commit";
        s = "status";
        co = "checkout";
        br = "branch";
        d = "diff";
        cbr = "rev-parse --abbrev-ref HEAD";
      };
    };
  };

  alacritty = {
    enable = true;
    settings = builtins.fromTOML (builtins.readFile ../shared/alacritty.toml);
  };

  zsh = {
    enable = true;
    autocd = true;
    plugins = [
    ];
    initExtra = ''
      [ -f /opt/dev/dev.sh ] && source /opt/dev/dev.sh

      [[ -f /opt/dev/sh/chruby/chruby.sh ]] && type chruby >/dev/null 2>&1 || chruby () { source /opt/dev/sh/chruby/chruby.sh; chruby "$@"; }

      [[ -x /opt/homebrew/bin/brew ]] && eval $(/opt/homebrew/bin/brew shellenv)

      export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
      export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
      export PATH="$HOME/.nix-profile/bin:$PATH"
      export PATH="$HOME/.local/bin:$PATH"
      export PATH="/nix/var/nix/profiles/default/bin:$PATH"
      export GPG_TTY=$(tty)

      source "$(fzf-share)/key-bindings.zsh"
      source "$(fzf-share)/completion.zsh"

      _gen_fzf_default_opts() {

      local color00='#282828'
      local color01='#3c3836'
      local color02='#504945'
      local color03='#665c54'
      local color04='#bdae93'
      local color05='#d5c4a1'
      local color06='#ebdbb2'
      local color07='#fbf1c7'
      local color08='#fb4934'
      local color09='#fe8019'
      local color0A='#fabd2f'
      local color0B='#b8bb26'
      local color0C='#8ec07c'
      local color0D='#83a598'
      local color0E='#d3869b'
      local color0F='#d65d0e'

      export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"\
      " --color=bg+:$color01,bg:$color00,spinner:$color0C,hl:$color0D"\
      " --color=fg:$color04,header:$color0D,info:$color0A,pointer:$color0C"\
      " --color=marker:$color0C,fg+:$color06,prompt:$color0A,hl+:$color0D"

      }

      _gen_fzf_default_opts

      source ${pkgs.zsh-fzf-tab}/share/fzf-tab/fzf-tab.plugin.zsh
      source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
    '';


    sessionVariables = {
      MCFLY_LIGHT = "TRUE";
      MCFLY_KEY_SCHEME = "VIM";
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#969896";
    };

    autosuggestion = {
      enable = true;
    };
    enableCompletion = true;
    shellAliases = {
      find = "fd";
      ls = "eza";
      l = "eza -l";
      la = "eza -la";
      cl = "clear";
      vim = "nvim";
      vi = "nvim";
    };

    oh-my-zsh = {
      enable = true;
      theme = "lambda";
      plugins = [
        "git"
        "vi-mode"
        "fzf"
        "colorize"
        "colored-man-pages"
        "command-not-found"
      ];
    };
  };

  neovim = {
    enable = true;
    vimAlias = true;
    extraConfig = builtins.readFile ./init.vim;
    extraLuaConfig = builtins.readFile luaCfg;
    plugins = with pkgs.vimPlugins; [
      vim-elixir
      nvim-treesitter
      vim-indent-guides
      vim-nix
      vim-signify
      zig-vim

      gruvbox-material
      gruvbox

      vim-commentary
      plenary-nvim
      vim-rhubarb

      vim-fugitive
      git-blame-nvim

      haskell-vim
      vim-ruby
      lightspeed-nvim
      vim-toml

      vim-vinegar
      vim-eunuch
      vim-surround
      vim-which-key
      vim-illuminate
      vim-test

      limelight-vim
      goyo-vim

      presenting-vim
      vim-abolish
      markdown-preview-nvim

      nvim-lspconfig
      fennel-vim
      nvim-cmp
      cmp-cmdline
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-vsnip
      vim-vsnip
      conflict-marker-vim
      lualine-nvim
      # ale
      wilder-nvim
      rust-vim
      telescope-nvim
      vim-liquid
      bigfile-nvim
      diffview-nvim
      tmux-navigator
      vim-graphql
      undotree
      vim-svelte
      vim-gruvbox8
      vim-markdown
      git-worktree-nvim
    ];
  };
} 
