#!/bin/bash
sudo apt-get -y remove emacs-*
sudo add-apt-repository -y ppa:cassou/emacs
sudo apt-get -y update
sudo apt-get -y install emacs-snapshot
sudo apt-get -y autoremove

rm -rf ~/.emacs.d
git clone --recursive https://github.com/nilnilnil/.emacs.d.git

emacs --debug-init
