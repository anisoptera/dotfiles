#!/usr/bin/env zsh

if [ "$(uname -s)" != "Darwin" ]
then
	sudo apt install maim ffmpeg xclip xininfo copyq
	mkdir ~/scratch
	git clone https://github.com/carnager/teiler.git ~/scratch/teiler
	pushd ~/scratch/teiler
	sudo make install
	popd
	rm -rf ~/scratch/teiler
fi
