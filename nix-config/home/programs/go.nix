{ config, pkgs, lib, ... }:

let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  go = unstable.go_1_17;
in
with lib;
{
  environment.systemPackages = with pkgs; [
    stdenv
    systemd
    glibc.static
    go
    gcc
  ];

  environment.variables = {
    CFLAGS="-I${pkgs.glibc.dev}/include -I${pkgs.systemd.dev}/include";
    LDFLAGS="-L${pkgs.glibc}/lib -L${pkgs.systemd.dev}/lib";
    GOROOT="${go}/share/go";
  };
}
