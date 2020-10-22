{ config, pkgs, ... }:

{

  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.

  boot.loader = {
    systemd-boot.enable = true;
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot";
    };

    grub = {
      enable = true;
      useOSProber = true;
      efiSupport = true;
      device = "nodev";
    };
  };

  networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp2s0.useDHCP = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "de_DE.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "de";
  };

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # List packages installed in system profile. To search, run:
  # $ nix search wget

  environment.systemPackages = with pkgs; [
    wget
    vim
    git
    htop
    alacritty
    google-chrome
    vscode
    emacs
    ripgrep
    fd
    findutils
    tdesktop
    dmenu
    nixfmt
    iosevka
    wmctrl
    feh
  ];

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services = {

    # Compositor Settings
    picom = {
      enable = true;
      shadow = true;
      inactiveOpacity = "1";
      backend = "glx";
      menuOpacity = "1";
      vSync = true;
    };
    xserver = {

      enable = true;
      layout = "de";
      videoDrivers = [ "nvidia" ];

      # Desktop Manager Settings
      desktopManager = {

        xterm.enable = false;

        # Window Manager Selection
        plasma5 = {
          enable = true;
          #enableXfwm = false;
        };
      };

      # Window Manager Settings
      windowManager = {

        i3 = {
          enable = true;
          package = pkgs.i3-gaps;
          extraPackages = with pkgs; [ dmenu i3status ];
        };
      };

      # Display Manager Settings
      displayManager = {

        sddm.enable = true;
        defaultSession = "plasma5+i3";
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.nix = {
    isNormalUser = true;
    extraGroups =
      [ "wheel" "vboxusers" "networkmanager" ]; # Enable ‘sudo’ for the user.
  };

  fonts.fonts = with pkgs; [ iosevka ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

  nixpkgs.config.allowUnfree = true;

}

