# alias for using emacs inside tmux
alias tmacs='TERM=xterm-256color emacs -nw'

# osx like copy/paste utils for linux!
if [[ $(uname -s) == Linux ]]
  then
    alias pbcopy='xclip -selection clipboard'
    alias pbpaste='xclip -selection clipboard -o'
fi

# rust musl builder, see https://github.com/emk/rust-musl-builder
alias rust-musl-nightly='docker run --rm -it -v "$(pwd)":/home/rust/src ekidd/rust-musl-builder:nightly'
alias rust-musl-stable='docker run --rm -it -v "$(pwd)":/home/rust/src ekidd/rust-musl-builder'

# twitch livestreamer auth -- public
alias livestreamer='livestreamer --http-header Client-ID=jzkbprff40iqj646a697cyrvl0zt2m6'

# fat fingers
gti() {
  git $@
}

random_bytes () {
  head -c "${1:-16}" < /dev/urandom | xxd -p
}

repeat () {
  while :; do clear && $1 && sleep $2; done
}

docker-rmi-none () {
  docker images | grep non | awk '{print $3}' | xargs -n 1 docker rmi
}
