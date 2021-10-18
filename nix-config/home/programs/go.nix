{ config, pkgs, lib, ... }:

let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in
with lib;
{
  environment.systemPackages = with pkgs; [
    stdenv
    systemd
    glibc.static
    unstable.go_1_17
  ];

  environment.variables = {
    CFLAGS="-I${pkgs.glibc.dev}/include -I${pkgs.systemd.dev}/include";
    LDFLAGS="-L${pkgs.glibc}/lib -L${pkgs.systemd.dev}/lib";
    GOROOT="${pkgs.go_1_16}/share/go";
  };
}
