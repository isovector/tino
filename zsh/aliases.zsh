alias ls='ls --color=tty'
alias diff='git diff --'
alias diffp='git diff --patience --'
alias pacman='sudo pacman'

alias calc='ghci'

alias ec2-serenade="ssh -i ~/.ssh/santino.pem ubuntu@$SERENADE"
alias ec2-haystack='ssh -i ~/.ssh/Haystack.pem ubuntu@52.10.229.25'

alias xcopy='xclip -selection c'

alias t='tino'
alias o='gnome-open'
alias e='nvim'
alias :e='nvim'

alias gs='git status'
alias ga='git add'
alias gc='git commit'

a() { alias $1="cd $PWD"; }

alias rs='redshift -O2500'
alias xs='redshift -x'

alias twsb='t w stack build'

function go() {
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
}

alias nix='nix-shell --run zsh'
