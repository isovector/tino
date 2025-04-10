# cat ~/.cache/wal/sequences
# cat ~/.config/wpg/sequences

zstyle ':completion:*' completer _complete _ignored _approximate
zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit
autoload -U add-zsh-hook
compinit
source <(jj util completion zsh)

HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
setopt extendedglob
setopt chaselinks
unsetopt autocd

export PROMPT_COMMAND='history -a; history -r'

for file in ~/.tino/zsh/*.zsh; do
    source $file
done
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
eval $(thefuck --alias)
eval "$(jump shell)"

case $TERM in
    xterm*)
        precmd () {print -Pn "\e]0;${PWD}\a"}
        ;;
esac

[ -f "/home/sandy/.ghcup/env" ] && source "/home/sandy/.ghcup/env" # ghcup-env

source <(fzf --zsh)

fzf-history-widget-accept() {
  fzf-history-widget
  # zle accept-line
}
zle     -N     fzf-history-widget-accept
bindkey '^R' fzf-history-widget-accept
