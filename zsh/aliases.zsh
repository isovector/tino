alias ls='ls --color=tty'
alias diff='git diff --'
alias diffp='git diff --patience --'
alias pacman='sudo pacman'

alias calc='stack ghci'

alias xcopy='xclip -selection c'

alias t='tino'
alias o='gnome-open'
alias e=$EDITOR
alias :e=$EDITOR

alias -g .st="\`stack ide targets 2>&1 | fzf\`"

alias gs='git status'
alias gd='git diff'
alias ga='git add'
alias gc='git commit && git push'
alias git-ignore='git update-index --assume-unchanged'
alias git-unignore='git update-index --no-assume-unchanged'
alias -g 'git commit'='echo "fuck you idiot"'
alias wire-cabal='git checkout **/*.cabal'
alias wire-vpn-start='systemctl start openvpn-client@sandymaguire'
alias wire-vpn-stop='systemctl stop openvpn-client@sandymaguire'

a() { alias $1="cd $PWD"; }


alias arbtt-today='arbtt-stats --filter='"'"'$sampleage <24:00'"'"
alias arbtt-week='arbtt-stats --filter='"'"'$sampleage <168:00'"'"
alias arbtt-help='arbtt-stats --exclude web: --exclude business --exclude conal --exclude slack: --exclude thinking --exclude scholarship --exclude call --exclude music: --exclude reddit: --exclude ta: --exclude pp --exclude prj: --exclude comm: --exclude tv: --exclude security --m=0 --filter='"'"'$sampleage <24:00'"'"' --dump-samples | fgrep '"'"'(*)'"'"'| uniq --count | sort --general-numeric-sort'

countdown() {
  clear;
  seconds=$1;
  date1=$((`date +%s` + $seconds));
  while [ "$date1" -ge `date +%s` ]; do
    echo -ne "\r$(date -u --date @$(($date1 - `date +%s` )) +%H:%M:%S) "; sleep 0.1s;
  done
}
