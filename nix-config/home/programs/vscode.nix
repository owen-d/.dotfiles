{ pkgs, lib, ... }:
# inspiration from
# https://nixos.wiki/wiki/Visual_Studio_Code
# https://matthewrhone.dev/nixos-vscode

# Extensions metadata can be created via the process specified in
# ./load-vscode-extensions.sh
# Unused, but here in case I figure it out in the future.

let
  extensions = (with pkgs.vscode-extensions; [
    ms-azuretools.vscode-docker
    ms-vscode-remote.remote-ssh
  ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace (import ./vscode-extensions.nix).extensions;
  vscode-with-extensions = pkgs.vscode-with-extensions.override {
    vscodeExtensions = extensions;
  };
in {
  config = {
    environment.systemPackages = [
      vscode-with-extensions
    ];

    system.activationScripts.fix-vscode-extensions = {
      text = ''
                EXT_DIR=/home/owen/.vscode/extensions
                mkdir -p $EXT_DIR
                chown owen:users $EXT_DIR
                for x in ${lib.concatMapStringsSep " " toString extensions}; do
                    ln -sf $x/share/vscode/extensions/* $EXT_DIR/
                done
                chown -R owen:users $EXT_DIR
            '';
      deps = [];
    };
  };
}
