shopt -s expand_aliases

. ~/.dotfiles/.bash_aliases

export HISTSIZE=20000
export HISTFILESIZE=20000

export EDITOR=vim

export GIT_SSH_COMMAND="ssh -i ~/.ssh/github_rsa"

export GOBIN="${HOME}/.local/go/bin"

pathappend "${HOME}/.local/bin" "${HOME}/.cargo/bin" "${GOBIN}"

export GPG_TTY=$(tty)

export NIXPKGS_ALLOW_UNFREE=1

case "$-" in
    # only runs for interactive shells
    *i*)
        . /etc/per-user/kube-ps1.sh
        export PS1='\W $(kube_ps1) $ '
        source <(kubectl completion bash 2>/dev/null)
        complete -F __start_kubectl kc 2>/dev/null
        ;;
esac

[ -f /etc/per-user/paths ] && . /etc/per-user/paths
[ -f ~/.keys.sh ] && . ~/.keys.sh
