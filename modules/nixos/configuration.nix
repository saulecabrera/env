{ lib, inputs, config, pkgs, ... }:


{
  imports =
    [
      ./hardware-configuration.nix
      inputs.nixos-hardware.nixosModules.framework-16-7040-amd
    ];


  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.extraModulePackages = [
    config.boot.kernelPackages.v4l2loopback.out
  ];

  boot.kernelModules = [
    "v4l2loopback"
  ];

  boot.kernelParams = ["amdgpu.sg_display=0"];

  boot.extraModprobeConfig = ''
   # https://github.com/umlaeute/v4l2loopback
    options v4l2loopback exclusive_caps=1 card_label="Virtual Camera"
  '';

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome = {
    enable = true;
    extraGSettingsOverridePackages = [ pkgs.mutter ];
    extraGSettingsOverrides = ''
        [org.gnome.mutter]
        experimental-features=['scale-monitor-framebuffer']
    '';
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "altgr-intl";
  };
  services.hardware.bolt.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Flatpak
  services.flatpak.enable = true;

  # Enable qmk and zsa.
  hardware.keyboard.zsa.enable = true;
  hardware.keyboard.qmk.enable = true;

  hardware.gpgSmartcards.enable = true;

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.saul = {
    isNormalUser = true;
    description = "Saúl Cabrera";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
    ];
    shell = pkgs.zsh;
  };

  fonts = {
    fontconfig = {
      antialias = true;
      cache32Bit = true;
      hinting.enable = true;
      hinting.autohint = true;
    };
    packages = with pkgs; [
      nerd-fonts.dejavu-sans-mono
    ];
  };

  # Install firefox.
  programs.firefox.enable = true;
  # Install zsh.
  programs.zsh.enable = true;


   programs.nix-ld = {
     enable = true;
     libraries = with pkgs; [
       llvmPackages.libclang
       clang
       glibc
     ];
   };

  services.pcscd.enable = true;
  services.udev.packages = [pkgs.yubikey-personalization];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    gnomeExtensions.arcmenu
    gnomeExtensions.dash-to-panel
    gnomeExtensions.forge
    gnomeExtensions.space-bar

    gcc_multi
    llvmPackages.clang
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  home-manager = {
    extraSpecialArgs = { inherit inputs; };
    users.saul.imports = [./home.nix];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
