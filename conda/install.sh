#!/bin/bash

which conda > /dev/null
if [ $? -ne 0 ]
then
	tmpfile=$(mktemp)
	if [[ $(uname) = 'Darwin' ]]
	then
		curl https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-x86_64.sh > $tmpfile
	else
		curl https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh > $tmpfile
	fi
	bash $tmpfile -b -u
fi
conda config --set changeps1 False
