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


export EDITOR="emacsclient"
eval $(dircolors -b)

[ -f ~/.zplug/init.zsh ] || (curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh)
source ~/.zplug/init.zsh

zplug "supercrabtree/k"
#zplug "changyuheng/zsh-interactive-cd"
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

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

AUTO_LS_COMMANDS=(ls)
auto-ls-ls(){
   ls --color=auto
   [[ $AUTO_LS_NEWLINE != false ]] && echo ""
}

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls'

if [ $+commands[fd] ]; then
	alias find=fd
fi

# git alias
alias gs="git status"
alias gd="git diff"
alias gp="git push"
alias gpull="git pull"
alias gc="git commit"
alias ga="git add"

# z.lua configuration
eval "$(lua ~/.zplug/repos/skywind3000/z.lua/z.lua --init zsh)"

autoload -U compinit && compinit -u

source ~/.zprofile
# zprof
