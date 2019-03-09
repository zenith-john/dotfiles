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
setopt CORRECT


export EDITOR="myemacs"
eval $(dircolors -b)

source ~/.zplug/init.zsh

zplug "changyuheng/zsh-interactive-cd"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-completions"
zplug "dannynimmo/punctual-zsh-theme", as:theme
zplug "zsh-users/zsh-history-substring-search"
zplug "MichaelAquilina/zsh-you-should-use"
zplug "Tarrasch/zsh-bd"
zplug "hcgraf/zsh-sudo"
zplug "desyncr/auto-ls"
# install wting/autojump manually

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

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

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

# Using /usr/bin/myemacs script instead

# my-emacs(){
#    RESULT=`ps -e | grep 'emacs'`
#    if [ -z "$RESULT" ]; then
#        /usr/bin/emacs
#    else
#        /usr/bin/emacsclient -c
#    fi
# }

alias emacs=myemacs

# added by Anaconda3 5.3.0 installer
# >>> conda init >>>
# !! Contents within this block are managed by 'conda init' !!
#__conda_setup="$(CONDA_REPORT_ERRORS=false '/home/zenith-john/anaconda3/bin/conda' shell.bash hook 2> /dev/null)"
#if [ $? -eq 0 ]; then
#    \eval "$__conda_setup"
#else
#    if [ -f "/home/zenith-john/anaconda3/etc/profile.d/conda.sh" ]; then
#        . "/home/zenith-john/anaconda3/etc/profile.d/conda.sh"
#        CONDA_CHANGEPS1=false conda activate base
#    else
#        \export PATH="/home/zenith-john/anaconda3/bin:$PATH"
#    fi
# fi
# unset __conda_setup
# <<< conda init <<<
#
# autojump configuration
[[ -s /home/zenith-john/.autojump/etc/profile.d/autojump.sh ]] && source /home/zenith-john/.autojump/etc/profile.d/autojump.sh
autoload -U compinit && compinit -u

source ~/.zprofile
# zprof
