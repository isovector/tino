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

alias gs='git status'
alias ga='git add'
alias gc='git commit'

a() { alias $1="cd $PWD"; }


function go() {
  cd $(
    for PIECE in "$@"; do
        if [[ -e "$PIECE" ]]; then
            cd $PIECE
        else
            WHERE=$(~/.tino/bin/tino place $PIECE)
            if [[ $? == 0 ]]; then
                eval WHERE=$WHERE
                cd $WHERE
            fi
        fi
    done
    pwd)
}

alias arbtt-today='arbtt-stats --filter='"'"'$sampleage <24:00'"'"
alias arbtt-week='arbtt-stats --filter='"'"'$sampleage <168:00'"'"
alias arbtt-help='arbtt-stats --exclude scholarship --exclude call --exclude music: --exclude reddit: --exclude pp --exclude prj: --exclude comm: --exclude tv: --exclude security --m=0 --filter='"'"'$sampleage <24:00'"'"' --dump-samples | fgrep '"'"'(*)'"'"'| uniq --count | sort --general-numeric-sort'
