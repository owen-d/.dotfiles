{ config, pkgs, lib, ... }:

with lib;
{
  environment.systemPackages = with pkgs; [
    emacs
    rust-analyzer
    goimports
  ];
}
