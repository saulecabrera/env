{config, pkgs, lib, ...}:

{
  services.emacs.enable = true;
  services.gpg-agent = {
    enable = true;
    # https://github.com/drduh/config/blob/master/gpg-agent.conf
    defaultCacheTtl = 60;
    maxCacheTtl = 120;
    pinentryPackage = pkgs.pinentry-curses;
    extraConfig = ''
      ttyname $GPG_TTY
    '';
  };
}
