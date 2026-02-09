# ---- Load alias map ----
typeset -A PATH_ALIASES
load_path_aliases() {
  PATH_ALIASES=()
  while read -r key val; do
    [[ -z "$key" || "$key" == \#* ]] && continue
    PATH_ALIASES[$key]="$val"
  done < ~/.path_aliases
}
load_path_aliases

# ---- Path shortening for prompt ----
shorten_path() {
  local path="$PWD" key val color_reset color_alias
  color_reset="%{$reset_color%}"
  color_alias="%{$fg[magenta]%}"  # pink / magenta tone

  for key val in ${(kv)PATH_ALIASES}; do
    # replace long paths with colored short ones
    path=${path//$val/${color_alias}${key}${fg[yellow]}}
  done

  print -- "$path"
}

# ---- Expand aliases for cd and global use ----
expand_path_alias() {
  local arg="$1" key val
  for key val in ${(kv)PATH_ALIASES}; do
    if [[ $arg == $key* ]]; then
      print -- "${arg/$key/$val}"
      return
    fi
  done
  print -- "$arg"
}


# Wrapper around cd
cd() {
  builtin cd "$(expand_path_alias "$1")"
}

# ---- Tab completion for @aliases ----
# Hybrid cd completion: offer both @aliases and normal paths
_expand_path_aliases_mixed() {
  # If we're completing the first word and it starts with '@', show aliases
  if [[ $PREFIX == @* ]]; then
    compadd ${(k)PATH_ALIASES}
  else
    # Fall back to regular cd completion
    _cd
  fi
}
compdef _expand_path_aliases_mixed cd

# ---- Global path expansion ----
# This hook rewrites any word beginning with '@' before execution.
expand_path_alias_hook() {
  local i expanded
  for i in {1..$#words}; do
    if [[ ${words[i]} == @* ]]; then
      expanded=$(expand_path_alias "${words[i]}")
      [[ -n $expanded ]] && words[i]=$expanded
    fi
  done
}
autoload -Uz add-zsh-hook
add-zsh-hook preexec expand_path_alias_hook

