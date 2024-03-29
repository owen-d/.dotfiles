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

alias kc='kubectl'

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

jsonnet-lint() {
    git diff --name-only | grep -E '(jsonnet|libsonnet)' | xargs -n 1 jsonnetfmt -i
}

pathappend() {
    for ARG in "$@"
    do
        if [ -d "$ARG" ] && [[ ":$PATH:" != *":$ARG:"* ]]; then
            PATH="${PATH:+"$PATH:"}$ARG"
        fi
    done
}

winekill() {
    ps aux | egrep 'wine(server|device|)' | grep -v grep | awk '{print $2}' | xargs -n 1 kill -9
}


oom-finder() {
    # usage: oom-finder <namespace> <name-label>
    kc -n "${1}" get pod -l name="${2}" -o json | jq '.items[] | select(.status.containerStatuses[].lastState.terminated.reason == "OOMKilled")' | jq '.metadata.name'
}

nix-clean () {
    nix-env --delete-generations old
    nix-store --gc
    nix-channel --update
    nix-env -u --always
    for link in /nix/var/nix/gcroots/auto/*
    do
        rm $(readlink "$link")
    done
    nix-collect-garbage -d
}
