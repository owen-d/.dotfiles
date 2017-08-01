# alias for using emacs inside tmux
alias tmacs='TERM=xterm-256color emacs -nw'

if [[ $(uname -s) == Linux ]]
  then
    alias pbcopy='xclip -selection clipboard'
    alias pbpaste='xclip -selection clipboard -o'
fi
