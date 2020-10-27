{ config, lib, pkgs, ... }:

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

  networking = {
    hostName = "nixos"; # Define your hostname.
    networkmanager.enable = true;
    useDHCP = false;
    interfaces.enp2s0.useDHCP = true;
    enableIPv6 = false;
  };

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
    unzip
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
    shutter
    kdeconnect
    plasma-browser-integration
    plasma-integration
    dropbox-cli
    zsh
    oh-my-zsh
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
      inactiveOpacity = "1.0";
      backend = "glx";
      menuOpacity = "1";
      vSync = true;
      settings = {
        shadow-red = 0.5;
        fading = true;
        sw-opti = true;
      };
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

  # -------------------------------------------------
  # User Config ----------------------------------
  # Define a user account. Don't forget to set a password with ‘passwd’.
  # -------------------------------------------------
  users.users.nix = {
    isNormalUser = true;
    extraGroups =
      [ "wheel" "vboxusers" "networkmanager" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  # -------------------------------------------------
  # Font Config ----------------------------------
  # -------------------------------------------------
  fonts.fonts = with pkgs; [
    iosevka
    corefonts # Microsoft free fonts
    fira # Monospace
    inconsolata # Monospace
    powerline-fonts
    ubuntu_font_family
    unifont # International languages
  ];

  # -------------------------------------------------
  # Dropbox Config ----------------------------------
  # -------------------------------------------------

  networking.firewall = {
    allowedTCPPorts = [ 17500 ];
    allowedUDPPorts = [ 17500 ];
  };

  systemd.user.services.dropbox = {
    description = "Dropbox";
    wantedBy = [ "graphical-session.target" ];
    environment = {
      QT_PLUGIN_PATH = "/run/current-system/sw/"
        + pkgs.qt5.qtbase.qtPluginPrefix;
      QML2_IMPORT_PATH = "/run/current-system/sw/"
        + pkgs.qt5.qtbase.qtQmlPrefix;
    };

    serviceConfig = {
      ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
      ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
      KillMode = "control-group"; # upstream recommends process
      Restart = "on-failure";
      PrivateTmp = true;
      ProtectSystem = "full";
      Nice = 10;
    };
  };

  # -------------------------------------------------
  # ZSH Config ----------------------------------
  # -------------------------------------------------
  programs.zsh = {
    enable = true;
    shellAliases = {
      #vim = "nvim";
      sn = "shutdown now";
    };
    enableCompletion = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    ohMyZsh = {
      enable = true;
      plugins = [ "git" "colored-man-pages" "command-not-found" "extract" ];
      theme = "agnoster";
    };

    promptInit = "";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

  nixpkgs.config.allowUnfree = true;

}

