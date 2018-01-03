# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config = {
    allowUnfree = true;
  };

  # Use the systemd-boot EFI boot loader.
  # boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sda2";
      preLVM = true;
      allowDiscards = true;
    }
  ];

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "nodev";
    efiSupport = true;
    gfxmodeEfi = "1024x768";
  };

  #networking.hostName = "nix"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    vim
    emacs
    git
    chromium
    vlc
    curl wget
    tmux
    zip unzip
    xcape
    xclip
  ];

  systemd.user.services."xcape" = {
    enable = true;
    description = "xcape to use CTRL as ESC when pressed alone";
    wantedBy = [ "default.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.xcape}/bin/xcape -e Control_L=Escape";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.bash.enableCompletion = true;
  programs.man.enable = true;
  programs.mosh.enable = true;
  programs.ssh.startAgent = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;


  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "mac";
  services.xserver.xkbOptions = "ctrl:nocaps, caps:ctrl_modifier";
  #services.xserver.multitouch.enable = true;
  #services.xserver.multitouch.invertScroll = true;

  services.xserver.displayManager.auto = {
    enable = true;
    user = "owen";
  };

  services.xserver.desktopManager.plasma5.enable = true;
  
  # Enable touchpad support.
  services.xserver.libinput.enable = true;
  services.xserver.libinput.naturalScrolling = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.owen = {
    extraGroups = ["wheel"];
    isNormalUser = true;
    uid = 1000;
  };
  
  security.sudo.configFile = ''
  # Don't edit this file. Set the NixOS options ‘security.sudo.configFile’
  # or ‘security.sudo.extraConfig’ instead.

  # Keep SSH_AUTH_SOCK so that pam_ssh_agent_auth.so can do its magic.
  Defaults env_keep+=SSH_AUTH_SOCK

  # "root" is allowed to do anything.
  root        ALL=(ALL:ALL) SETENV: ALL

  # Users in the "wheel" group can do anything.
  %wheel      ALL=(ALL:ALL) NOPASSWD:SETENV: ALL

  # Keep terminfo database for root and %wheel.
  Defaults:root,%wheel env_keep+=TERMINFO_DIRS
  Defaults:root,%wheel env_keep+=TERMINFO

  '';

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

}


