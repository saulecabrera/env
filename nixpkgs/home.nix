{ config, pkgs, lib, ... }:

let
  # Compile Fennel to Lua
  luaCfg = pkgs.runCommandLocal "init.lua" {} ''
    ${pkgs.fennel}/bin/fennel --compile ${./init.fnl} > $out
  '';
in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "saulecabrera";
  home.homeDirectory = "/Users/saulecabrera";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";

  home.packages = with pkgs; [
    tig
    shellharden
    nix-prefetch-git
    fd
    haskellPackages.hspec-discover
    nodejs
    hugo
    # python3
    # python39Packages.pip
    ripgrep
    figlet
    slides
    graph-easy
    yarn
    elixir
    tree
    nasm
    ttf_bitstream_vera
    qemu
    vistafonts
    dejavu_fonts
    fennel
    fnlfmt
    jrnl
    glow
    zsh-fzf-tab
    rustup
    binaryen
    gnupg
    pinentry_mac
    trunk
    jq
  ];

  programs.tmux = {
    enable = true;
    extraConfig = builtins.readFile ./tmux.conf;
    plugins = with pkgs.tmuxPlugins; [
      fuzzback
      vim-tmux-navigator
      tmux-fzf
    ];
  };


  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs.eza.enable = true;

  programs.bat = {
    enable = true;
    config = {
      style = "plain";
      italic-text = "always";
      theme = "ansi";
      pager = "less -XFr";
    };
  };

  programs.git = {
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

  programs.zsh = {
    enable = true;
    autocd = true;
    plugins = [
      {
        name = "powerlevel10k";
        src = pkgs.zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
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


      export FZF_DEFAULT_OPTS="
    	  --color=fg:#797593,bg:#faf4ed,hl:#d7827e
      	--color=fg+:#575279,bg+:#f2e9e1,hl+:#d7827e
	      --color=border:#dfdad9,header:#286983,gutter:#faf4ed
	      --color=spinner:#ea9d34,info:#56949f,separator:#dfdad9
	      --color=pointer:#907aa9,marker:#b4637a,prompt:#797593"

      export TMUX_FZF_OPTIONS="
    	  --color=fg:#797593,bg:#faf4ed,hl:#d7827e
      	--color=fg+:#575279,bg+:#f2e9e1,hl+:#d7827e
	      --color=border:#dfdad9,header:#286983,gutter:#faf4ed
	      --color=spinner:#ea9d34,info:#56949f,separator:#dfdad9
	      --color=pointer:#907aa9,marker:#b4637a,prompt:#797593"

      source ${pkgs.zsh-fzf-tab}/share/fzf-tab/fzf-tab.plugin.zsh
      source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
      source $HOME/Developer/env/nixpkgs/.p10k.zsh
    '';


    sessionVariables = {
      MCFLY_LIGHT = "TRUE";
      MCFLY_KEY_SCHEME = "VIM";
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#969896";
    };

    enableAutosuggestions = true;
    enableCompletion = true;
    shellAliases = {
      ls = "eza";
      l = "eza -l";
      la = "eza -la";
      cl = "clear";
      vim = "nvim";
      vi = "nvim";
      e = "open /Applications/Emacs.app";
      jn = "jrnl";
      jr = "jrnl -1 --edit";
    };

    oh-my-zsh = {
      enable = true;
      theme = "lambda";
      plugins = [
        "git"
        "z"
        "vi-mode"
        "fzf"
        "command-not-found"
      ];
    };
  };

  programs.neovim = {
    enable = true;
    vimAlias = true;
    extraConfig = builtins.readFile ./init.vim;
    extraLuaConfig = builtins.readFile luaCfg;
    plugins = with pkgs.vimPlugins; [
      seoul256-vim
      vim-elixir
      nvim-treesitter
      vim-indent-guides
      vim-nix
      vim-signify
      zig-vim

      gruvbox-material
      gruvbox
      vim-solarized8
      papercolor-theme
      nord-vim

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
      everforest
      ale
      wilder-nvim
      rust-vim
      telescope-nvim
      vim-liquid
      rose-pine
      bigfile-nvim
      diffview-nvim
      tmux-navigator
      kanagawa-nvim
      vim-graphql
    ];
  };
}

