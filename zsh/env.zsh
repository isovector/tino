export SERENADE='52.7.77.9'
export MARLO='45.56.95.176'
export MX='172.105.22.231'
export FUNKY='172.232.163.190'

export NIX_IGNORE_SYMLINK_STORE=1

export GIT_EDITOR="nvim"
export EDITOR="nvim"
export VISUAL="nvim"
export LESS="FRK"
export PAGER="less"

export PYTHONPATH=$PYTHONPATH:/var/lib
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
export NVIM_GTK_NO_HEADERBAR=1

# export LD_PRELOAD="/opt/hdf5110/lib/libhdf5.so:$LD_RELOAD"

export REVIEW_BASE=main

eval "$(direnv hook zsh)"

# export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix'
export FZF_DEFAULT_COMMAND='ag -l -g ""'
export FZF_ALT_C_COMMAND="find . -type d -not -path '*/.*'"
export TERM=xterm-256color
export ESCDELAY=100
