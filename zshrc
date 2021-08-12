#!/bin/zsh

# for the use of tmux
bindkey -e

# zmodload zsh/zprof
setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY # Don't execute immediately upon history expansion.
HISTFILE=$HOME/.zsh-history
HISTSIZE=1000
SAVEHIST=1000

# Set up stack
DIRSTACKSIZE=10
setopt autopushd pushdminus pushdsilent pushdtohome pushdignoredups cdablevars
alias d='dirs -v | head -10'

zstyle ':completion:*' menu select=1

setopt AUTO_MENU
setopt AUTO_CD
setopt EXTENDED_GLOB
#setopt CORRECT
fpath+=~/.zfunc

export EDITOR="emacsclient -c"
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=gasp'
eval $(dircolors -b)

[ -f ~/.zplug/init.zsh ] || (curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh)
source ~/.zplug/init.zsh

zplug "zenith-john/k"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-completions"
zplug "dannynimmo/punctual-zsh-theme", as:theme
zplug "zsh-users/zsh-history-substring-search"
zplug "MichaelAquilina/zsh-you-should-use"
zplug "Tarrasch/zsh-bd"
zplug "hcgraf/zsh-sudo"
zplug "desyncr/auto-ls"
zplug "skywind3000/z.lua"

if ! zplug check ; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load

bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward

[ -f ~/.fzf.zsh ] || (git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install)
source ~/.fzf.zsh

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

alias e="TERM=xterm-256color emacsclient -nw -c"
alias em="emacs -nw > /dev/null"
alias ek="emacsclient -e \"(kill-emacs)\""

alias o="xdg-open"

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    alias ls='ls --color=auto'
fi

AUTO_LS_COMMANDS=(ls)
auto-ls-ls(){
    ls --color=auto
    [[ $AUTO_LS_NEWLINE != false ]] && echo ""
}

under(){
    mv "$1" "${1// /_}"
}

# some more ls aliases
alias ll='ls -ahlF'
alias la='ls -Ah'
alias l='ls'

clean(){
    rm -i *.bbl || true
    rm -i *.bcf || true
    rm -i *.aux || true
    rm -i *.log || true
    rm -i *.blg || true
    rm -i *.run.xml || true
    rm -i *.synctex.gz || true
}

if [ $+commands[ccat] ]; then
    alias c=ccat
fi

# git alias
if [ $+commands[git] ]; then
    alias gs="git status"
    alias gd="git diff"
    alias gp="git push"
    alias gpull="git pull"
    alias gc="git commit"
    alias ga="git add"
    alias gget="git submodule add --depth=1"
fi

alias pip=pip3
# z.lua configuration
export _ZL_MATCH_MODE=1
eval "$(lua ~/.zplug/repos/skywind3000/z.lua/z.lua --init zsh)"
alias zp="z -I | fzf | awk '{print \$2}'"

# load GMXRC
if [ -f /usr/local/gromacs/bin/GMXRC ]; then
    source "/usr/local/gromacs/bin/GMXRC"
fi

# source /usr/share/nvm/init-nvm.sh

export PATH="$HOME/.pyenv/bin:$PATH"
export TERM="xterm-256color"
export COLORTERM=truecolor

autoload -U compinit && compinit -u
# zprof
