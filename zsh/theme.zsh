autoload colors; colors;

setopt PROMPT_SUBST

function places() {
  NAME=${PWD/$HOME/'~'}
  cat ~/.places ~/.places.local | awk '{ print length(), $0 | "sort -rn | cut -d\\  -f2-" }' | \
  while read LINE; do
      SUB=$(echo $LINE | tr -s ' ' | cut -d' ' -f2)
      WITH=%{${fg[blue]}%}$(echo $LINE | tr -s ' ' | cut -d' ' -f1)%{$reset_color$fg[yellow]%}
      NAME=${NAME/$SUB/$WITH}
  done
  echo $NAME
}

function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(git rev-parse --short HEAD 2> /dev/null) || return
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

parse_git_dirty() {
  local SUBMODULE_SYNTAX=''
  if [[ $POST_1_7_2_GIT -gt 0 ]]; then
        SUBMODULE_SYNTAX="--ignore-submodules=dirty"
  fi
  if [[ -n $(git status -s ${SUBMODULE_SYNTAX}  2> /dev/null) ]]; then
    echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
  else
    echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
  fi
}

PROMPT='
%{$fg_bold[red]%}➜%{$reset_color%}  $(git_prompt_info)%{$fg[yellow]%}$(places)%{$reset_color%} '

RPROMPT='%{$fg[green]%}%T%{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[red]%}%{$reset_color%}%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[red]%}+"
ZSH_THEME_GIT_PROMPT_CLEAN=""

if [ -e "$HOME/.zshrc.theme.local" ]; then
    source ~/.zshrc.theme.local
fi
