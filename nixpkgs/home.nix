{ config, pkgs, lib, ... }:

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
    shellharden
    nix-prefetch-git
    fd
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
    yarn
    elixir
    tree
    nasm
    rust-analyzer
    ttf_bitstream_vera
    qemu
    vistafonts
    dejavu_fonts
    fennel
  ];

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs.fzf.enable = true;
  programs.exa.enable = true;

  programs.kitty = {
    enable = true;

    # darwinLaunchOptions = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin [
    #   "--single-instance"
    #   "--directory=/tmp/my-dir"
    #   "--listen-on=unix:/tmp/my-socket"
    # ];

    settings = {
      disable_ligatures = "always";


        foreground =		"#d4be98";
        background =		"#1d2021";
        cursor	=		"#a89984";
        cursor_text_color =	"background";
        color0	=		"#665c54";
        color8	=		"#928374";
        color1	=		"#ea6962";
        color9	=		"#ea6962";
        color2	=		"#a9b665";
        color10=		"#a9b665";
        color3	=		"#e78a4e";
        color11=		"#d8a657";
        color4	=		"#7daea3";
        color12=		"#7daea3";
        color5	=		"#d3869b";
        color13=		"#d3869b";
        color6	=		"#89b482";
        color14=		"#89b482";
        color7	=		"#d4be98";
        color15=		"#d4be98";

        active_tab_background = "#1d2021";
        active_tab_foreground = "#d4be98";
        active_tab_font_style = "bold";
        inactive_tab_background = "#1d2021";
        inactive_tab_foreground = "#a89984";
        inactive_tab_font_style = "normal";
    };

    font.name = "PragmataPro Mono Liga";
    font.size = 15;
  };

  programs.bat = {
    enable = true;
    config = {
      style = "plain";
      italic-text = "always";
      theme = "gruvbox-dark";
      pager = "less -XFr";
    };
  };

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
      github.copilot
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
      core.editor = "hx";
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
      ls = "exa";
      l = "exa -l";
      la = "exa -la";
      cl = "clear";
      vim = "nvim";
      vi = "nvim";
      e = "open /Applications/Emacs.app";
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
        vim-elixir
        orgmode
        nvim-treesitter
        fzf-vim
        vim-indent-guides
        vim-nix
        vim-airline
        vim-airline-themes
        vim-signify
        zig-vim

        gruvbox
        NeoSolarized
        vim-gruvbox8
        base16-vim
        papercolor-theme
        nord-vim

        vim-commentary
        plenary-nvim
        neogit
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

        neoterm
        presenting-vim
        vim-abolish
        vim-smoothie
        markdown-preview-nvim
        gruvbox-material

        # At the time of writing this is generating an error when
        # runnin `home-manager switch`
        # aniseed
        nvim-lspconfig
        fennel-vim
        nvim-cmp
        cmp-cmdline
        cmp-nvim-lsp
        cmp-buffer
        cmp-path
        todo-comments-nvim
        cmp-vsnip
        vim-vsnip
      ];
  };
}

