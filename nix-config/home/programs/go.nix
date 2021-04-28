{ config, pkgs, lib, ... }:

with lib;
{
  environment.systemPackages = with pkgs; [
    stdenv
    systemd
    glibc.static
    go_1_16
  ];

  environment.variables = {
    CFLAGS="-I${pkgs.glibc.dev}/include -I${pkgs.systemd.dev}/include";
    LDFLAGS="-L${pkgs.glibc}/lib -L${pkgs.systemd.dev}/lib";
  };
}
