#!/bin/sh
if test ! $(which npm)
then
	node/nvm-install.sh
	export NVM_DIR="$HOME/.nvm"
	[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
	nvm install node
fi

if test ! $(which spoof)
then
  npm install spoof -g
fi
