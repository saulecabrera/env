{ config, pkgs, ... }:

{
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
    mcfly
    nix-prefetch-git
    fd
    haskellPackages.ghcup
    haskellPackages.hspec-discover
    nodejs
    rustup
    hugo
    python3
    python39Packages.pip
    ripgrep
    figlet
    slides
    graph-easy
  ];

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs.fzf.enable = true;
  programs.exa.enable = true;

  programs.bat = {
    enable = true;
    config = {
      style = "plain";
      italic-text = "always";
      theme = "Solarized (light)";
      pager = "less -XFr";
    };
  };

  # Install manually:
  #
  # PaperColor
  programs.vscode = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
      matklad.rust-analyzer
      vscodevim.vim
      vspacecode.vspacecode
      vspacecode.whichkey
      yzhang.markdown-all-in-one
      zhuangtongfa.material-theme
      haskell.haskell
      bbenoist.nix
    ];
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
    # Installing llvm through brew
    initExtra = ''
      [ -f /opt/dev/dev.sh ] && source /opt/dev/dev.sh

      [[ -f /opt/dev/sh/chruby/chruby.sh ]] && type chruby >/dev/null 2>&1 || chruby () { source /opt/dev/sh/chruby/chruby.sh; chruby "$@"; }

      [[ -x /opt/homebrew/bin/brew ]] && eval $(/opt/homebrew/bin/brew shellenv)

      eval "$(mcfly init zsh)"

      export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
      export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
      export PATH="$HOME/.nix-profile/bin:$PATH"
      export PATH="$HOME/.local/bin:$PATH"
    '';


    sessionVariables = {
      MCFLY_LIGHT = "TRUE";
      MCFLY_KEY_SCHEME = "VIM";
    };
    enableAutosuggestions = true;
    enableCompletion = true;
    shellAliases = {
      ls = "exa";
      l = "exa -l";
      la = "exa -la";
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
        "z"
        "git"
        "command-not-found"
      ];
    };
  };

  programs.neovim = {
    enable = true;
    vimAlias = true;
    extraConfig = builtins.readFile ./init.vim;
    plugins = with pkgs.vimPlugins; [
      fzf-vim
      vim-nix
      vim-airline
      vim-airline-themes
      vim-gitgutter
      zig-vim

      gruvbox
      NeoSolarized
      vim-gruvbox8
      base16-vim
      papercolor-theme
      nord-vim

      coc-nvim

      vim-commentary
      plenary-nvim
      neogit
      vim-rhubarb
      vim-fugitive
      git-blame-nvim

      haskell-vim
      vim-ruby
      hop-nvim
      vim-toml

      vim-vinegar
      vim-eunuch
      vim-surround
      vim-which-key
      vim-illuminate
      vim-test

      limelight-vim
      goyo-vim

      neoterm
      presenting-vim
      vim-diminactive
      vim-abolish
    ];
  };
}

