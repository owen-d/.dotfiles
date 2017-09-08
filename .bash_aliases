# alias for using emacs inside tmux
alias tmacs='TERM=xterm-256color emacs -nw'

# osx like copy/paste utils for linux!
if [[ $(uname -s) == Linux ]]
  then
    alias pbcopy='xclip -selection clipboard'
    alias pbpaste='xclip -selection clipboard -o'
fi

# rust musl builder, see https://github.com/emk/rust-musl-builder
alias rust-musl-builder='docker run --rm -it -v "$(pwd)":/home/rust/src ekidd/rust-musl-builder'
