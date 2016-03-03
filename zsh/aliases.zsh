alias ls='ls --color=tty'
alias diff='git diff --'
alias diffp='git diff --patience --'
alias pacman='sudo pacman'

alias calc='python'

alias ec2-serenade="ssh -i ~/.ssh/santino.pem ubuntu@$SERENADE"
alias ec2-haystack='ssh -i ~/.ssh/Haystack.pem ubuntu@52.10.229.25'

alias xcopy='xclip -selection c'

alias t='tino'
alias o='gnome-open'
alias v='vim'
alias e='vim'
alias :e='vim'

alias gs='git status'
alias ga='git add'
alias gc='git commit'

a() { alias $1="cd $PWD"; }
