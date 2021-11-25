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
  home-manager = builtins.fetchGit {
    url = "https://github.com/nix-community/home-manager.git";
    rev = "91bd34620d73340be03642279ee0d1c64110ee6c";
    ref = "release-20.09";
  };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      (import "${home-manager}/nixos")
      "${programs}/emacs.nix"
      "${programs}/go.nix"
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 20;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/305b9f52-3b4a-49e1-acf8-1e64a3aa364e";
      preLVM = true;
    };
  };

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

  virtualisation.docker.enable = true;

  # Enable the Plasma 5 Desktop Environment.
  services = {

    xserver = {
      enable = true;
      videoDrivers = [ "nvidia" ];
      windowManager.xmonad = {
        config = (import "${dotuser}/xmonad.nix");
        enableContribAndExtras = true;
        extraPackages = hpkgs: with hpkgs; [ dbus monad-logger xmonad-contrib ];
        enable = true;
      };

      serverFlagsSection = ''
        Option "BlankTime" "0"
        Option "StandbyTime" "0"
        Option "SuspendTime" "0"
        Option "OffTime" "0"
      '';

      xrandrHeads = [ "DP-0" "HDMI-0" ];

      displayManager = {

        sessionCommands = ''
          ${pkgs.xorg.setxkbmap}/bin/setxkbmap -option ctrl:nocaps -option caps:ctrl_modifier
          ${pkgs.xcape}/bin/xcape -e Control_L=Escape
          ${pkgs.xlibs.xset}/bin/xset r rate 185 50
        '';

        lightdm = {
          background = "${dotuser}/media/img/sunset.jpg";
          enable = true;
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
  };

  home-manager = {
    useGlobalPkgs = true;
    users.owen = { pkgs, ... }: {

      xresources.extraConfig = (import "${dotuser}/xresources.nix");
      services.polybar = {
        enable = true;
        # pulseSupport = true;
        script = "polybar main &";
        config = (import "${programs}/polybar.nix");
      };
    };

  };

  # Should be removable in the future
  # https://github.com/NixOS/nixpkgs/pull/86480/files
  hardware.opengl.driSupport32Bit = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.owen = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
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
    unstable.slack
    steam
    discord
    xmonad-log
    dmenu # search util
    jq
    unstable.kubectl
    unstable.xwallpaper
    ripgrep
    unstable.rustup
    bash-completion
    gcc
    pkg-config
    zlib
    openssl
    cargo-edit
    gopls
    git-crypt
    gnupg
    google-cloud-sdk
    awscli2
    azure-cli
    lutris
    graphviz
    gnome3.gnome-screenshot
    transmission
    stig # transmission tui
    tree
    discord
    jsonnet
    unstable.gh
    shfmt
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
    (import "${dotuser}/cargo.nix").files
    (import "${dotuser}/kube-ps1.nix").files
  ];

  system.userActivationScripts = {
    linking = lib.strings.concatStrings (lib.strings.intersperse "\n" [
      ''
        ln -sfn /etc/per-user/alacritty/alacritty.yml ~/.alacritty.yml
        ln -sfn "${dots}/.tmux.conf" ~/.tmux.conf
        ln -sfn "${dots}/.bashrc" ~/.bashrc
      ''
      (import "${dotuser}/gitconfig.nix").linking.text
      (import "${dotuser}/cargo.nix").linking.text
    ]);

  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "tty";
  };

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

