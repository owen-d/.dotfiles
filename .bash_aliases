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
  while :;
  do
      local t="$1"
      local output="$(${@:2} 2>&1)"
      clear && echo "${output}" && sleep $t
  done
}

docker-rmi-none () {
  docker images | grep non | awk '{print $3}' | xargs -n 1 docker rmi
}

mem() {
    ps -eo rss,pid,euser,args:100 --sort %mem | grep -v grep | grep -i $@ | awk '{printf $1/1024 "MB"; $1=""; print }'
}

gwd() {
    cd ${GOPATH}/src/github.com/owen-d
}
