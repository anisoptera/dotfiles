#!/bin/sh
if test ! $(which bb)
then
    sudo clojure/babashka-install
fi
