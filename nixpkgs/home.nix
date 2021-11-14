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
    ccls
    fd
  ];

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
        st = "status";
        co = "checkout";
        br = "branch";
        d = "diff";
        cbr = "rev-parse --abbrev-ref HEAD";
      };
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

      gruvbox
      NeoSolarized
      vim-gruvbox8
      base16-vim
      papercolor-theme
      nord-vim

      coc-nvim

      vim-commentary
      neogit
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

      vim-floaterm
      vim-test

      limelight-vim
      goyo-vim
    ];
  };

  programs.fish = {
    enable = true;
    shellInit = builtins.readFile ./config.fish;
  };
}

