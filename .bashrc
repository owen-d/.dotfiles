shopt -s expand_aliases

. ~/.dotfiles/.bash_aliases

export EDITOR=vim

export GIT_SSH_COMMAND="ssh -i ~/.ssh/github_rsa"

pathappend "${HOME}/.local/bin" "${HOME}/.cargo/bin"

export GPG_TTY=$(tty)

source <(kubectl completion bash 2>/dev/null)
complete -F __start_kubectl kc 2>/dev/null

[ -f /etc/per-user/paths ] && . /etc/per-user/paths
