zstyle ':completion:*' completer _complete _ignored _approximate
zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit
autoload -U add-zsh-hook
compinit

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt extendedglob
setopt chaselinks
unsetopt autocd

export PROMPT_COMMAND='history -a; history -r'

for file in ~/.tino/zsh/*.zsh; do
    source $file
done
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
eval $(thefuck --alias)
