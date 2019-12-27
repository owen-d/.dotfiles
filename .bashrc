shopt -s expand_aliases

. ~/.dotfiles/.bash_aliases

export GIT_SSH_COMMAND="ssh -i ~/.ssh/github_rsa"

# go
export GOPATH=~/go
export PATH="${PATH}:${GOPATH}/bin"

source $HOME/.cargo/env

export GPG_TTY=$(tty)
