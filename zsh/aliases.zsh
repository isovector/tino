alias ls='ls --color=tty'
alias diff='git diff --'
alias diffp='git diff --patience --'
alias pacman='sudo pacman'

alias calc='(cd && stack ghci)'

alias xcopy='xclip -selection c'

alias t='tino'
alias o='gnome-open'
# alias e='jj edit'
# alias :e=$EDITOR

e() {
  if (( $# != 0 )); then
    jj edit $*
  else
    REV=$(jj log --color always -T 'description.remove_suffix("\n")++" $"++change_id.shortest()' | fzf --ansi | cut -d'$' -f2)
    jj edit $REV
  fi
}

alias -g .st="\`stack ide targets 2>&1 | fzf\`"

alias hag='ag -G "hs$"'
alias gs='jj st'
alias gd='jj diff'
alias git-ignore='git update-index --assume-unchanged'
alias git-unignore='git update-index --no-assume-unchanged'

alias l='jj -r'

alias cs='jj -r changeset'
alias prs='jj -r avalanche'
alias plans="jj -r 'downstream(trunk(), sigil_plan) | sandy-root'"
alias format="new Format && ./tools/format.sh -f all"
alias golden="new Golden; add-parent dev-root; stack run -- --all --golden --update; rm-parent dev-root"
alias jp='jj git push -r cap'

new() { jj new -B cap -m "$*" }
bnew() { jj new -B @ -m "$*" }
newb() { jj new -B @ -m "$*" }
desc() { jj describe -m "$*" }
a() { alias $1="cd $PWD"; }
add-parent() { jj rebase -s @ -d "all:@- | ($1)" }
rm-parent() { jj rebase -s @ -d "all:@- & ~($1)" }
rebase-main() { jj git fetch && jj rebase -s sandy-root -d 'trunk()' }
stack-pr() { jj new -r 'heads(@::)' -m "$*" }
new-pr() { jj new -r dev-root -m "$*" }

avalancheimpl() {
  PRS=$(gh pr list)
  echo '```'
  while read LINE; do
    HEAD=$(echo $LINE | cut -d'#' -f1)
    BRANCH=$(echo $LINE | cut -d'#' -f2)
    echo $LINE | grep '#' &> /dev/null
    if [ "$?" -eq "0" ]; then
      PR=$(echo $PRS | grep $BRANCH | cut -f1)
      echo -n $HEAD
      echo -n '#'
      echo $PR
    else
      echo $LINE
    fi
  done < <(jj -r 'branches & (sandy-root::)' -T '" "++description.remove_suffix("\n")++" #"++ branches')
  echo '```'
}

avalanche() {
  avalancheimpl | sed 's/∅ #/main/'
}


alias arbtt-today='arbtt-stats -o this-day --filter='"'"'$sampleage <24:00'"'"
alias arbtt-week='arbtt-stats -o this-week --filter='"'"'$sampleage <168:00'"'"
alias arbtt-backlog='arbtt-stats -o last-week -o prj:maniga --filter='"'"'$sampleage <336:00'"'"
alias arbtt-help='arbtt-stats --exclude web: --exclude business --exclude conal --exclude slack: --exclude thinking --exclude scholarship --exclude work: --exclude call --exclude music: --exclude reddit: --exclude ta: --exclude pp --exclude prj: --exclude comm: --exclude tv: --exclude security --m=0 --filter='"'"'$sampleage <24:00'"'"' --dump-samples | fgrep '"'"'(*)'"'"'| uniq --count | sort --general-numeric-sort'
alias arbtt-work='arbtt-week -o this-week -o prj:maniga -x inactive | head -n4 | tail -n1'

countdown() {
  clear;
  seconds=$1;
  date1=$((`date +%s` + $seconds));
  while [ "$date1" -ge `date +%s` ]; do
    echo -ne "\r$(date -u --date @$(($date1 - `date +%s` )) +%H:%M:%S) "; sleep 0.1s;
  done
}
