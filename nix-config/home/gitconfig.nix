{
  files = {
    "per-user/gitconfig".text = ''
      # This is Git's per-user configuration file.
      [user]
              name = Owen Diehl
              email = ow.diehl@gmail.com
      # Please adapt and uncomment the following lines:
      #       name = Owen Diehl
      #       email = ow.diehl@gmail.com
      [alias]
              hist = log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short
      [init]
              templatedir = ~/.git-templates
      [pull]
              ff = only
      # [commit]
      #         gpgsign = true
      [url "git@github.com:"]
              insteadOf = https://github.com/
    '';

    "per-user/git-templates/hooks/commit-msg".text = ''
      #!/bin/sh

      grep '^Signed-off-by: ' "$1" || \
      echo "Signed-off-by: $(git config --get user.name) <$(git config --get user.email)>" >>"$1"
    '';
  };

  linking = {
    text = ''
      ln -sfn /etc/per-user/gitconfig ~/.gitconfig
      mkdir -p ~/.git-templates/hooks
      ln -sfn /etc/per-user/git-templates/hooks/commit-msg ~/.git-templates/hooks/commit-msg
    '';
  };
}
