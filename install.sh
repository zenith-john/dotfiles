#!/usr/bin/sh

if type "rcup" > /dev/null; then
	echo "Creating Symbolic links"
    rcup
fi

if type "git" > /dev/null; then
	echo "Add git global ignore"
	git config --global core.excludesfile '~/.gitignore'
fi

rm ~/.install.sh
