export SERENADE='52.7.77.9'

export GIT_EDITOR="nvim"
export EDITOR="nvim"
export VISUAL="nvim"
export PAGER="less"

if [[ -n ${TMUX} && -n ${commands[tmux]} ]];then
  case $(tmux showenv TERM 2>/dev/null) in
    *256color) ;&
    TERM=fbterm) TERM=screen-256color ;;
    *) TERM=screen
  esac
fi

export NIX_CONF_DIR="/home/sandy/.nix"
