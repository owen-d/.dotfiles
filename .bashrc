shopt -s expand_aliases

. ~/.dotfiles/.bash_aliases

export GIT_SSH_COMMAND="ssh -i ~/.ssh/github_rsa"

pathappend "${HOME}/.local/bin" "${HOME}/.cargo/bin"

export GPG_TTY=$(tty)

source <(kubectl completion bash)
complete -F __start_kubectl kc

[ -f /etc/per-user/paths ] && . /etc/per-user/paths
