shopt -s expand_aliases

. ~/.dotfiles/.bash_aliases

export GIT_SSH_COMMAND="ssh -i ~/.ssh/github_rsa"

# go
export GOPATH=~/go
export PATH="${PATH}:${GOPATH}/bin"
export PATH="${PATH}:${HOME}/.local/bin"

source $HOME/.cargo/env

export GPG_TTY=$(tty)

source <(kubectl completion bash)
complete -F __start_kubectl kc
