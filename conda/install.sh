#!/bin/bash

which conda > /dev/null
if [ $? -ne 0 ]
then
	tmpfile=$(mktemp)
	curl https://repo.continuum.io/miniconda/Miniconda3-latest-MacOSX-x86_64.sh > $tmpfile
	bash $tmpfile
fi
conda config --set changeps1 False
