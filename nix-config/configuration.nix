# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  dots =  "/home/owen/.dotfiles";
  nixdots = "${dots}/nix-config";
  dotuser = "${nixdots}/home";
  programs = "${dotuser}/programs";
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      "${programs}/emacs.nix"
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
  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    windowManager.xmonad = {
      config = (import "${dotuser}/xmonad.nix");
      enableContribAndExtras = true;
      extraPackages = hpkgs: with hpkgs; [ dbus monad-logger xmonad-contrib ];
      enable = true;
    };
    displayManager = {
      # ssdm.enable = true;
      # plasma5.enable = true;
      lightdm = {
        enable = true;
        background = "${dotuser}/home/media/img/akira0.jpg";
        greeters.gtk = {
          enable = true;
          iconTheme = {
            name = "Papirus";
            package = pkgs.papirus-icon-theme;
          };
          # theme = {
          #   name = "fortuneteller2k_phocus";
          #   package = pkgs.phocus;
          # };
        };
      };
      defaultSession = "none+xmonad";
    };
  };


  # Should be removable in the future
  # https://github.com/NixOS/nixpkgs/pull/86480/files
  hardware.opengl.driSupport32Bit = true;

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

  # random svc for init stuff I need
  systemd.user.services."x-user-init" = {
    description = "random x configs";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    script = ''
      ${pkgs.xlibs.xset}/bin/xset r rate 185 50
      ${pkgs.xlibs.xrandr}/bin/xrandr --output DP-0 --pos 0x0
      ${pkgs.xlibs.xrandr}/bin/xrandr --output DP-4 --pos 3440x0
    '';
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
    xmonad-log
    dmenu # search util
    jq
    slack
    kubectl
    unstable.xwallpaper
    ripgrep
    cargo
    rustup
  ];

  environment.etc = builtins.foldl' lib.trivial.mergeAttrs {
      "per-user/alacritty/alacritty.yml".text =
        # This is some hacky stuff :/
        # Basically the "\x1bx" is the (M-x) keyset but it gets ripped out
        # via the yaml function, which actually uses JSON under the hood...
        # So, we re-inject it in post-processing and pray theres not another
        # x1bx in the file :D
        lib.strings.escape
          ["x1bx"]
          (lib.generators.toYAML {} (import "${dotuser}/alacritty.nix"));
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

