# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

export PATH="/home/zenith-john/.cargo/bin/:/home/zenith-john/anaconda3/bin/:$PATH"


# Unset manpath so we can inherit from /etc/manpath via the `manpath` command
unset MANPATH # delete if you already modified MANPATH elsewhere in your config
export MANPATH="$NPM_PACKAGES/share/man:$(manpath)"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

[[ -s /home/zenith-john/App/amber18/amber.sh ]] && source /home/zenith-john/App/amber18/amber.sh

export MC_XDG_OPEN=~/.local/bin/nohup-open

# when installing pdf-tools
# avoid using the ananconda pkg-config
export PKG_CONFIG_PATH=/usr/bin
export PKG_CONFIG=/usr/bin/pkg-config

# export z.lua for ranger
export RANGER_ZLUA=~/.zplug/repos/skywind3000/z.lua/z.lua
