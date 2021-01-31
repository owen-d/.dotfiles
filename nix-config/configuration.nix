# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

let
  dots =  "/home/owen/.dotfiles";
  nixdots = "${dots}/nix-config";
  dotuser = "${nixdots}/home";
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nixpkgs.config = {
    allowUnfree = true;
  };

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/New_York";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp3s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the Plasma 5 Desktop Environment.
  services.xserver.enable = true;
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];

  # Should be removable in the future
  # https://github.com/NixOS/nixpkgs/pull/86480/files
  hardware.opengl.driSupport32Bit = true;

  systemd.user.services."x-user-init" = {
    # Note: this is b/c I couldn't get the following working:
    # services.xserver.autoRepeatDelay = 185;
    # services.xserver.autoRepeatInterval = 50;
    enable = true;
    description = "hacking around x configs at startup (not working via exposed nixos xserver options)";
    wantedBy = [ "default.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.xlibs.xset}/bin/xset r rate 185 50";
  };

  systemd.user.services."xcape" = {
    enable = true;
    description = "xcape to use CTRL as ESC when pressed alone";
    wantedBy = [ "default.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStartPre = "${pkgs.xorg.setxkbmap}/bin/setxkbmap -option ctrl:nocaps -option caps:ctrl_modifier";
    serviceConfig.ExecStart = "${pkgs.xcape}/bin/xcape -e Control_L=Escape";
  };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.owen = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  security.sudo.wheelNeedsPassword = false;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    emacs
    git
    google-chrome
    vlc
    curl
    wget
    tmux
    zip unzip
    xcape
    xclip
    streamlink
    alacritty
    lsof
    gnumake
    gcc
    binutils
    clang
    slack
    steam
    discord
  ];

  environment.etc = builtins.foldl' lib.trivial.mergeAttrs {
      "per-user/alacritty/alacritty.yml".text = lib.generators.toYAML {} (import "${dotuser}/alacritty.nix");
    } [
    (import "${dotuser}/gitconfig.nix").files
  ];

  system.userActivationScripts = {
    linking = lib.strings.concatStrings (lib.strings.intersperse "\n" [
      ''
        ln -sfn /etc/per-user/alacritty/alacritty.yml ~/.alacritty.yml
        ln -sfn /etc/per-user/.gitconfig ~/.gitconfig
      ''
      (import "${dotuser}/gitconfig.nix").linking.text
    ]);
    # linking = {
    #   text = ''
    #     ln -sfn /etc/per-user/alacritty/alacritty.yml ~/.alacritty.yml
    #     ln -sfn /etc/per-user/.gitconfig ~/.gitconfig
    #   '';
    #   deps = [];
    # };
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

