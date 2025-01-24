{config, pkgs, lib, ...}:

{
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
  
  # services.hypridle = {
  #   enable = true;
  #   settings = {
  #     general = {
  #       before_sleep_cmd = "loginctl lock-session";
  #       after_sleep_cmd = "hyprctl dispatch dpms on";
  #       lock_cmd = "pidof hyprlock || hyprlock";
  #     };
  #     listener = [
  #       {
  #         timeout = 120;
  #         on-timeout = "hyprlock";
  #       }
  #     ];
  #   };
  # };
}
