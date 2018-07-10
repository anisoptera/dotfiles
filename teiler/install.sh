#!/usr/bin/zsh

sudo apt install maim ffmpeg xclip
mkdir ~/scratch
git clone https://github.com/carnager/teiler.git ~/scratch/teiler
pushd ~/scratch/teiler
sudo make install
popd
rm -rf ~/scratch/teiler
