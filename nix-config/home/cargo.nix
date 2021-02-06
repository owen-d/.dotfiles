{
  files = {
    "per-user/cargo/config.toml".text = ''
      [net]
      git-fetch-with-cli = true   # use the `git` executable for git operations
    '';
  };

  linking = {
    text = ''
      mkdir -p ~/.cargo
      ln -sfn /etc/per-user/cargo/config.toml ~/.cargo/config.toml
    '';
  };
}
