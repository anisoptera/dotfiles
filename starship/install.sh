#!/bin/sh
if test ! $(which starship)
then
	starship/starship-install.sh -y
fi
