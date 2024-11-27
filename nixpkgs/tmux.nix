{pkgs, ...}:

{
    enable = true;
    extraConfig = builtins.readFile ./tmux.conf;
    plugins = with pkgs; [
      pkgs.tmuxPlugins.tmux-fzf
      pkgs.tmuxPlugins.fuzzback
      pkgs.tmuxPlugins.vim-tmux-navigator
    ];
}
